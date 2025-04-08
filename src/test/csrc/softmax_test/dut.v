module dut(
 input [31:0] instr,
 input clock,
 input reset,
 output reg ready
);

wire vle;
assign vle= (instr[6:0]==7'b000_0111) && instr[14];
reg [32*32-1:0] vreg [0:31];

wire [4:0] rd;
assign rd=instr[11:7];

import "DPI-C" function void pmem_read(
    input  longint unsigned paddr,
    output int unsigned    output_bits[32]
);

int rdata [0:31];
always @(posedge clock)
if(reset) begin
    for(int i=0;i<31 ;i++) begin
        rdata[i]<=0;
        vreg[i] <=0;
    end
end
else if (vle) begin
    pmem_read(64'd0,rdata);
    for(int i=0;i<32 ;i++)
        vreg[rd][i*32 +: 32] <= rdata[i];
end

always @(posedge clock)
if(reset)
    ready<=1'b0;
else if (vle) begin
    ready<=1'b1;
end
else begin
    ready<=1'b0;
end

import "DPI-C" function void get_vreg(input logic [1023:0] data [0:31]);

always@(*)
if(ready==1) begin
    get_vreg(vreg);
end


endmodule