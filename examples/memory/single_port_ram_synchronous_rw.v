module ram_sp_sr_sw(clk, addrIn, dataIn, we, dataOut, oe);
	parameter DATA_WIDTH = 4;
	parameter ADDR_WIDTH = 4;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
		
	input clk;
	input [ADDR_WIDTH-1:0] addrIn;
	input [DATA_WIDTH-1:0] dataIn;
	input we;
	input oe;
	output [DATA_WIDTH-1:0] dataOut;
	
	reg [ADDR_WIDTH-1:0] addrIn;
	reg [DATA_WIDTH-1:0] dataIn;
	reg [DATA_WIDTH-1:0] dataOut;
	reg [DATA_WIDTH-1:0] mem [0:RAM_DEPTH-1];
	
	always @ (posedge clk)
	begin
		if(we) begin
			mem[addrIn] = dataIn;
		end
	end
	
	always @ (posedge clk)
	begin
		if(!we && oe) begin
			dataOut = mem[addrIn];
		end
	end
endmodule 