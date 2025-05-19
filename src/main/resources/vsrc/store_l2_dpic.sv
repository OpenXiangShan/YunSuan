module store_l2_dpic #(
  parameter VLEN = 1024  // 定义VLEN
)(
  input  logic         clk,
  input  logic         enable,
  input  logic [63:0]  paddr,
  input  logic [VLEN-1:0]  store_data
);

  logic [31:0]  store_data_wire[VLEN/32-1:0];
  // 将store_data输入连接到store_data_wire数组
  genvar i;
  generate
    for (i = 0; i < VLEN/32; i = i + 1) begin : gen_store_data
      assign store_data_wire[i] = store_data[i*32+:32];
    end
  endgenerate

  // DPI-C导入声明
  import "DPI-C" function void pmem_write(input longint unsigned paddr, input int unsigned input_bits[VLEN/32]);

  // 存储输出位的临时数组
  int temp_output_bits[VLEN/32];
  

  // 当enable有效时调用C函数
  always_ff @(posedge clk) begin
    if (enable) begin
      for (int i = 0; i < VLEN/32; i++) begin
        temp_output_bits[i] = store_data_wire[i];
      end
      pmem_write(paddr, temp_output_bits);
    end
  end


endmodule 