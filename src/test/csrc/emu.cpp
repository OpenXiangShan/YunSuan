
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <getopt.h>
#include "VSimTop.h"
#include "verilated_vcd_c.h"

#include "include/test_driver.h"
#include "include/emu.h"

static inline long long int atoll_strict(const char *str, const char *arg) {
  if (strspn(str, " +-0123456789") != strlen(str)) {
    printf("[ERROR] --%s=NUM only accept numeric argument\n", arg);
    exit(EINVAL);
  }
  return atoll(str);
}

static inline void print_help(const char *file) {
  printf("Usage: %s [OPTION...]\n", file);
  printf("\n");
  printf("  -s, --seed=NUM             use this seed\n");
  printf("  -C, --max-cycles=NUM       execute at most NUM cycles\n");
  printf("  -O, --max-operations=NUM   execute at most NUM instructions\n");
  printf("  -b, --log-begin=NUM        display log from NUM th cycle\n");
  printf("  -e, --log-end=NUM          stop display log at NUM th cycle\n");
  printf("      --dump-wave            dump waveform when log is enabled\n");
  printf("  -h, --help                 print program help info\n");
  printf("\n");
}

inline EmuArgs parse_args(int argc, const char *argv[]) {
  EmuArgs args;
  int long_index = 0;
  const struct option long_options[] = {
    { "dump-wave",         0, NULL,  0  },
    { "seed",              1, NULL, 's' },
    { "max-cycles",        1, NULL, 'C' },
    { "max-operations",    1, NULL, 'O' },
    { "log-begin",         1, NULL, 'b' },
    { "log-end",           1, NULL, 'e' },
    { "help",              0, NULL, 'h' },
    { 0,                   0, NULL,  0  }
  };

  int o;
  while ( (o = getopt_long(argc, const_cast<char *const*>(argv),
          "-s:C:O:h:b:e:", long_options, &long_index)) != -1) {
    switch (o) {
      case 0:
        switch (long_index) {
          case 0: args.enable_waveform = true; continue;
        }
      default:
        print_help(argv[0]);
        exit(0);
      case 's':
        if(std::string(optarg) != "NO_SEED") {
          args.seed = atoll_strict(optarg, "seed");
          printf("Using seed = %d\n", args.seed);
        }
        break;
      case 'C': args.max_cycles = atoll_strict(optarg, "max-cycles");  break;
      case 'O': args.max_operations = atoll_strict(optarg, "max-operations");  break;
      case 'b': args.log_begin = atoll_strict(optarg, "log-begin");  break;
      case 'e': args.log_end = atoll_strict(optarg, "log-end"); break;
    }
  }

  Verilated::commandArgs(argc, argv); // Prepare extra args for TLMonitor
  return args;
}

Emulator::Emulator(int argc, const char *argv[]):
  dut_ptr(new VSimTop), cycles(0), operations(0)
{
  args = parse_args(argc, argv);

  srand(args.seed);
  srand48(args.seed);

  Verilated::randReset(2);

#if VM_TRACE == 1
  if (args.enable_waveform) {
    Verilated::traceEverOn(true);
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);
    time_t now = time(NULL);
    tfp->open(waveform_filename(now,"_dump_wave"));
  }
#endif
  
  test_driver.set_default_value(dut_ptr);
  reset_ncycles(10);
}

Emulator::~Emulator() {
  return;
}


int Emulator::single_cycle() {

  dut_ptr->clock = 0;
  dut_ptr->eval();

  // clock posedge, assign value
  test_driver.assign_input_raising(dut_ptr);

  #if VM_TRACE == 1
    if (args.enable_waveform) {
      if (args.log_begin <= cycles && cycles <= args.log_end) {
        tfp->dump(cycles);
      }
    }
    cycles ++;
  #endif


  dut_ptr->clock = 1;
  dut_ptr->eval();

  #if VM_TRACE == 1
    if (args.enable_waveform) {
      if (args.log_begin <= cycles && cycles <= args.log_end) {
        tfp->dump(cycles);
      }
    }
  #endif
  cycles ++;
  // check result
  int trap_code = test_driver.diff_output_falling(dut_ptr);
  if (trap_code == STATE_FINISH_OPERATION || trap_code == STATE_BADTRAP) {
    operations += 1;
  }
  return trap_code;
}

void Emulator::dummy_single_cycle() {
  dut_ptr->clock = 0;
  dut_ptr->eval();
  #if VM_TRACE == 1
    if (args.enable_waveform) {
      if (args.log_begin <= cycles && cycles <= args.log_end) {
        tfp->dump(cycles);
      }
    }
  cycles ++;
  #endif
  dut_ptr->clock = 1;
  dut_ptr->eval();
  #if VM_TRACE == 1
    if (args.enable_waveform) {
      if (args.log_begin <= cycles && cycles <= args.log_end) {
        tfp->dump(cycles);
      }
    }
  #endif
  cycles ++;
}

void Emulator::reset_ncycles(size_t cycle) {
  for (int i = 0; i < cycle; i++) {
    dut_ptr->reset = 1;
    dut_ptr->clock = 0;
    dut_ptr->eval();
    #if VM_TRACE == 1
      tfp->dump(cycles);
    #endif
    cycles++;
    dut_ptr->clock = 1;
    dut_ptr->eval();
    #if VM_TRACE == 1
      tfp->dump(cycles);
    #endif
    cycles++;
  }
  dut_ptr->reset = 0;
}

int Emulator::execute_operations(uint64_t ops) {
  int trap_code;
  for (int op = 0; op < ops; ) {
    trap_code = single_cycle();
    if ((trap_code == STATE_FINISH_OPERATION) || (trap_code == STATE_BADTRAP)) op ++;
    if ((trap_code != STATE_RUNNING) && (trap_code != STATE_FINISH_OPERATION)) return op;
  }
  return ops;
}

bool Emulator::execute() {
  int trap_code;
  while ((cycles < args.max_cycles) && (operations < args.max_operations)) {
    trap_code = single_cycle();
    if ((trap_code != STATE_RUNNING) && (trap_code != STATE_FINISH_OPERATION)) {
      // after bad trap, dump more 10 cycles
      for(int post=0; post<5; post++) dummy_single_cycle();
      break;
    }
  }

#if VM_TRACE == 1
  if (args.enable_waveform) tfp->close();
#endif

  bool good_trap = ((trap_code == STATE_RUNNING) || (trap_code == STATE_FINISH_OPERATION));
  printf("EMU %s\n", good_trap ? "EXCEEDED LIMIT" : "BADTRAP");
  printf("HAS Executed Cycles:%ld Operations:%ld\n", cycles, operations);
  if (!good_trap) {
    printf("=========Re-Execute, Print Golden Model Trace and Dump Wave===========\n");

#if VM_TRACE == 1
    args.log_begin = 0; args.log_end = -1;
    args.enable_waveform = true;
    Verilated::traceEverOn(true);
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);
    time_t now = time(NULL);
    tfp->open(waveform_filename(now,"_re_execute"));
#endif
    cycles = 0;
    test_driver.verbose_exec();
    test_driver.keep_input();
    reset_ncycles(5);
    test_driver.gen_next_test_case(); // set issued true -> false
    execute_operations(1);
    for(int post=0; post<5; post++) dummy_single_cycle();
#if VM_TRACE == 1
    tfp->close();
#endif
  }

  // single_cycle(); // one more cycle to print result
  return good_trap;
}

inline char* Emulator::timestamp_filename(time_t t, char *buf) {
  char buf_time[64];
  char cwd[128];
  strftime(buf_time, sizeof(buf_time), "%F@%T", localtime(&t));
  getcwd(cwd, 64);
  int len = snprintf(buf, 1024, "%s/build/%s", cwd, buf_time);
  return buf + len;
}

inline char* Emulator::waveform_filename(time_t t, const char *s) {
  static char buf[1024];
  char *p = timestamp_filename(t, buf);
  char suffix[100];
  snprintf(suffix, 100, "%s_seed_%d.vcd", s, args.seed);
  snprintf(p, 1024, "%s", suffix);
  printf("dump wave to %s...\n", buf);
  return buf;
}