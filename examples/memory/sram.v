module sram(clk, addr, we, dataIn, dataOut);
	input clk,we;
	input [1:0] addr;
	input [1:0] dataIn;
	output [1:0] dataOut;

	reg [1:0] dataOut;
	reg [1:0] mem [0:3];
	
	always @(posedge clk)
	begin
		dataOut = (!we) ? mem[addr] : dataOut;
		if(we)
			mem[addr] = dataIn;
	end
endmodule