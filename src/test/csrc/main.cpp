#include "VSimTop.h"
#include <getopt.h>
#include <signal.h>
#include <unistd.h>
#include <iostream>
#include <sys/time.h>
#include <functional>

#include "include/emu.h"


// junk, link for verilator
std::function<double()> get_sc_time_stamp = []() -> double { return 0; };
double sc_time_stamp() { return get_sc_time_stamp(); }

int main(int argc, const char** argv) {
  printf("Emu compiled at %s, %s\n", __DATE__, __TIME__);

  struct timeval begin, end;
  gettimeofday(&begin, NULL);

  auto emu = new Emulator(argc, argv);

  bool good_trap = emu->execute();
  delete emu;
  
  gettimeofday(&end, NULL);
  int s = end.tv_sec - begin.tv_sec;
  int us = end.tv_usec - begin.tv_usec;
  if (us < 0) { s --; us += 1000000; }

  printf("EMU has executed %d ms.\n", s * 1000 + (us + 500) / 1000);

  return !good_trap;
}