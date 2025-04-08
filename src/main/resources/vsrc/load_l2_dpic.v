module load_l2_dpic #(
  parameter VLEN = 2048  // 定义VLEN
)(
  input  logic         clk,
  input  logic         rst_n,
  input  logic         enable,
  input  logic [63:0]  paddr,
  output logic [VLEN-1:0]  load_data,
  output logic         load_valid
);

  logic [31:0]  load_data_wire[VLEN/32-1:0];
  // 将load_data_wire数组连接到load_data输出
  genvar i;
  generate
    for (i = 0; i < VLEN/32; i = i + 1) begin : gen_load_data
      assign load_data[i*32+:32] = load_data_wire[i];
    end
  endgenerate
  

  // DPI-C导入声明
  import "DPI-C" function void pmem_read(input longint paddr, output int output_bits[]);

  // 存储输出位的临时数组
  int temp_output_bits[VLEN/32];
  
  // valid信号控制
  logic valid_r;
  always_ff @(posedge clk) begin
    if (enable) begin
      valid_r <= 1'b1;
    end else begin
      valid_r <= 1'b0;
    end
  end
  
  // 当enable有效时调用C函数
  always_ff @(posedge clk) begin
    if (enable) begin
      pmem_read(paddr, temp_output_bits);
      for (int i = 0; i < VLEN/32; i++) begin
        load_data_wire[i] <= temp_output_bits[i];
      end
    end
  end
  
  // 输出valid信号
  assign load_valid = valid_r;

endmodule 