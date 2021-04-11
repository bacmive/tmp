module ram_sp_sr_sw(clk, we, re, addr, wdata, rdata);
	parameter DATA_WIDTH = 4;
	parameter ADDR_WIDTH = 4;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
	
	input clk, we, re;
	input [ADDR_WIDTH-1:0] addr;
	input [DATA_WIDTH-1:0] wdata;
	output [DATA_WIDTH-1:0] rdata;

	reg [DATA_WIDTH-1:0] rdata;
	reg [DATA_WIDTH-1:0] mem[0:RAM_DEPTH-1];

	always @(posedge clk) begin
		rdata = re ? mem[addr]: rdata;
		if (we)
			mem[addr] = wdata;
	end
endmodule 