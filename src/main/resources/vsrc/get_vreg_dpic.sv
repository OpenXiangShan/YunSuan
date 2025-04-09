// Module that provides an interface to the get_vreg DPI-C function.
// This module receives register address and data segments as inputs
// and calls the C function when enabled.
module get_vreg_dpic #(
    parameter VLEN = 2048 // Vector length, should match C code and hardware config
) (
    input clk,          // Clock input
    input enable,       // Enable signal to trigger the DPI call

    // Inputs corresponding to the get_vreg DPI function arguments
    input [7:0]          rf_addr_in,          // 8-bit signed register address
    input [VLEN-1:0]     data_0_in,
    input [VLEN-1:0]     data_1_in,
    input [VLEN-1:0]     data_2_in,
    input [VLEN-1:0]     data_3_in,
    input [VLEN-1:0]     data_4_in,
    input [VLEN-1:0]     data_5_in,
    input [VLEN-1:0]     data_6_in,
    input [VLEN-1:0]     data_7_in
    // Note: No output ports are defined as the DPI function returns void.
    // Add outputs like 'done' if needed based on external logic requirements.
);

    logic [31:0] data_0_in_vec [VLEN/32-1:0];
    logic [31:0] data_1_in_vec [VLEN/32-1:0];
    logic [31:0] data_2_in_vec [VLEN/32-1:0];
    logic [31:0] data_3_in_vec [VLEN/32-1:0];
    logic [31:0] data_4_in_vec [VLEN/32-1:0];
    logic [31:0] data_5_in_vec [VLEN/32-1:0];
    logic [31:0] data_6_in_vec [VLEN/32-1:0];
    logic [31:0] data_7_in_vec [VLEN/32-1:0];

    // Convert input data to 32-bit wide array format for passing to the DPI-C function
    genvar i;
    generate
        for (i = 0; i < VLEN/32; i = i + 1) begin : gen_data_vec
            assign data_0_in_vec[i] = data_0_in[i*32+:32];
            assign data_1_in_vec[i] = data_1_in[i*32+:32];
            assign data_2_in_vec[i] = data_2_in[i*32+:32];
            assign data_3_in_vec[i] = data_3_in[i*32+:32];
            assign data_4_in_vec[i] = data_4_in[i*32+:32];
            assign data_5_in_vec[i] = data_5_in[i*32+:32];
            assign data_6_in_vec[i] = data_6_in[i*32+:32];
            assign data_7_in_vec[i] = data_7_in[i*32+:32];
        end
    endgenerate


    // DPI-C function import declaration
    // Note: VLEN/32 determines the size of each data_N array based on VLEN.
    import "DPI-C" function void get_vreg(
        input byte rf_addr,                 // 8-bit signed register address
        input int unsigned data_0[VLEN/32], // 32-bit unsigned integer arrays
        input int unsigned data_1[VLEN/32],
        input int unsigned data_2[VLEN/32],
        input int unsigned data_3[VLEN/32],
        input int unsigned data_4[VLEN/32],
        input int unsigned data_5[VLEN/32],
        input int unsigned data_6[VLEN/32],
        input int unsigned data_7[VLEN/32]
    );

    // Call the DPI-C function when enabled
    // Using always_ff for sequential logic, assuming the DPI call should happen
    // synchronously with the clock edge when enabled.
    // If the call should be purely combinational based on 'enable',
    // an always_comb block could be used, but this might have simulation
    // implications depending on the C function's behavior.
    always_ff @(posedge clk) begin
            if (enable) begin
                // Call the imported DPI-C function with the module inputs
                get_vreg(
                    rf_addr_in,
                    data_0_in_vec,
                    data_1_in_vec,
                    data_2_in_vec,
                    data_3_in_vec,
                    data_4_in_vec,
                    data_5_in_vec,
                    data_6_in_vec,
                    data_7_in_vec
                );
                 // Optional: Add debug display message
                 // $display("%t: DPI function get_vreg called with rf_addr = %d", $time, rf_addr_in);
            end
    end

endmodule 