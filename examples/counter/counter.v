module counter ( clk, rst, dout);
	parameter MSBD = 1;
    
    input clk, rst;
    output [MSBD:0] dout;
    reg [1:0] last;
    
    always@ (posedge clk) begin
        if (rst)
            last = (MSBD+1)'b0;
        else begin
            last = last + 1;
        end
    end
	
	assign dout = last;
	
endmodule
