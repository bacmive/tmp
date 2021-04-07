module counter ( clk, rst, dout);
    output [1:0] dout;
    input clk, rst;
    
    reg [1:0] dout
    
    always@ (posedge clk) begin
        if (rst)
            dout = 2'b0;
        else begin
            dout = dout + 1;
        end
    end
endmodule
