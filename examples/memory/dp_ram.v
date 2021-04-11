module dp_ram (clka, clkb, 
			   wea, rea, addra, dina,
			   web, reb, addrb, dinb, 
			   douta, doutb );
	parameter DATA_WIDTH = 2;
	parameter ADDR_WIDTH = 2;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
	
	input clk, wea, web, rea, reb;
	input [ADDR_WIDTH-1:0] addra;
	input [DATA_WIDTH-1:0] dina;
	input [ADDR_WIDTH-1:0] addrb;
	input [DATA_WIDTH-1:0] dinb;
	output [DATA_WIDTH-1:0] douta;
	output [DATA_WIDTH-1:0] doutb;
	
	reg [DATA_WIDTH-1:0] douta;
	reg [DATA_WIDTH-1:0] doutb;
	reg [DATA_WIDTH-1:0] mem [0:RAM_DEPTH-1];
	
	always @(posedge clka)begin
        if(!wea)begin
			mem[addra] = dina;
		end
		douta = (rea) ? mem[addra] : douta;
	end
	 ​
	always @(posedge clkb)begin
		if(!web)begin
			mem[addrb] = dinb;
		end
		doutb = (reb) ? mem[addrb] : doutb;
	end

endmodule