// Simulation-only stand-in for the IHP SRAM macro (same ports): synchronous
// read and bit-masked write on A_CLK when A_MEN is high. BIST ports ignored.
// Synthesis and place-and-route use the real macro from the PDK.
module RM_IHPSG13_1P_256x48_c2_bm_bist (
    input A_CLK, input A_MEN, input A_WEN, input A_REN,
    input [7:0] A_ADDR, input [47:0] A_DIN, input A_DLY,
    output reg [47:0] A_DOUT, input [47:0] A_BM,
    input A_BIST_CLK, input A_BIST_EN, input A_BIST_MEN, input A_BIST_WEN,
    input A_BIST_REN, input [7:0] A_BIST_ADDR, input [47:0] A_BIST_DIN, input [47:0] A_BIST_BM
);
  reg [47:0] mem[0:255];
  always @(posedge A_CLK)
    if (A_MEN) begin
      if (A_WEN) mem[A_ADDR] <= (mem[A_ADDR] & ~A_BM) | (A_DIN & A_BM);
      if (A_REN) A_DOUT <= mem[A_ADDR];
    end
endmodule
