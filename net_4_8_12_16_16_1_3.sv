module net_4_8_12_16_16_1_3(clk, reset, input_valid, input_ready, input_data, output_valid, output_ready, output_data);
   input clk, reset, input_valid, output_ready;
   input signed [15:0] input_data;
   output logic output_valid, input_ready;
   output logic signed [15:0] output_data;	
   logic signed [15:0] output_data_l1, output_data_l2;	
   logic output_valid_l1, output_valid_l2, input_ready_l2, input_ready_l3;
   l1_fc_8_4_16_1_1 l1_fc_8_4_16_1_1(clk, reset, input_valid, input_ready, input_data, output_valid_l1, input_ready_l2, output_data_l1);
   l2_fc_12_8_16_1_1 l2_fc_12_8_16_1_1(clk, reset, output_valid_l1, input_ready_l2, output_data_l1, output_valid_l2, input_ready_l3, output_data_l2);
   l3_fc_16_12_16_1_1 l3_fc_16_12_16_1_1(clk, reset, output_valid_l2, input_ready_l3, output_data_l2, output_valid, output_ready, output_data);
endmodule

module l1_fc_8_4_16_1_1(clk, reset, input_valid, input_ready, input_data, output_valid_l1, input_ready_l2, output_data_l1);
   input clk, reset, input_valid, input_ready_l2;
   input signed [15:0] input_data;
   output logic signed [15:0] output_data_l1;
   output logic output_valid_l1, input_ready;
   fc_8_4_16_1_1_l1 fc_8_4_16_1_1_l1(clk, reset, input_valid, input_ready, input_data, output_valid_l1, input_ready_l2, output_data_l1);
endmodule

module fc_8_4_16_1_1_l1(clk, reset, input_valid, input_ready, input_data, output_valid_l1, input_ready_l2, output_data_l1);
   	input clk, reset, input_valid, input_ready_l2;
   	input signed [15:0] input_data;
   	output logic signed [15:0] output_data_l1;
   	output logic output_valid_l1, input_ready;
   	logic clear_acc_l1_l1, en_acc_l1;
   	logic [1:0] addr_x_l1;
   	logic [4:0] addr_w_1_l1;
   	logic wr_en_x_l1;
   	logic cl_x_l1;
   	logic cl_addr_l1;
   	logic en_mult_l1;
   	logic multiplex_count_l1;
   	logic [1:0] state_l1;
   	data_path_1 data_path_1(clk, reset, input_data, addr_x_l1, wr_en_x_l1,addr_w_1_l1,	clear_acc_l1, en_acc_l1, output_data_l1, cl_x_l1, cl_addr_l1, en_mult_l1, multiplex_count_l1, state_l1);
   	control_1 control_1(clk, reset, input_valid, input_ready_l2, addr_x_l1, wr_en_x_l1,addr_w_1_l1,	clear_acc_l1, en_acc_l1, input_ready, output_valid_l1, cl_x_l1, cl_addr_l1, en_mult_l1, multiplex_count_l1, state_l1);
endmodule

module data_path_1(clk, reset, input_data, addr_x_l1, wr_en_x_l1,addr_w_1_l1,clear_acc_l1, en_acc_l1, output_data_l1, cl_x_l1, cl_addr_l1, en_mult_l1, multiplex_count_l1, state_l1); 
   	input clk, reset;
   	input signed [15:0] input_data;
   	output logic signed [15:0] output_data_l1;
   	logic signed [15:0] output_data_1_l1;
   	input clear_acc_l1, en_acc_l1;
   	input wr_en_x_l1;
   	input [1:0] addr_x_l1;
   	input [4:0] addr_w_1_l1;
   	input cl_x_l1, cl_addr_l1;
   	input en_mult_l1;
   	logic signed [15:0] x_out_l1, w_out_1_l1;
   	logic signed [15:0] acc_1_l1;
   	logic signed [15:0] mult_1_l1;
   	logic signed [31:0] mult_1_l1_temp;
   	input multiplex_count_l1;
   	input [1:0] state_l1;
   	parameter [1:0] load_l1=2'b00, compute_l1=2'b01, stall_l1=2'b10, out_l1=2'b11;
   	l1_fc_8_4_16_1_1_W_rom_1 mem_w_1(clk, addr_w_1_l1, w_out_1_l1);
   	mem_x_l1 mem_x_l1(clk, input_data, x_out_l1, addr_x_l1, wr_en_x_l1);

   	always_ff @(posedge clk)begin
   		 if((reset == 1) || (clear_acc_l1 == 1))begin
   			 output_data_1_l1 <= 0;
   		 end
   		 else if(en_acc_l1 == 1)begin
   			 output_data_1_l1 <= acc_1_l1;
   		 end
   	end
   	always_ff @(posedge clk)begin
   		 if(en_mult_l1 == 1) begin
   			 mult_1_l1_temp <= w_out_1_l1 * x_out_l1;
   		 end
   		 else begin
   			 mult_1_l1_temp <= 0;
   		 end
   	end
   	always_comb begin
         if(mult_1_l1_temp >= 16'sb0111111111111111)
             mult_1_l1 = 16'sb0111111111111111;
         else if(mult_1_l1_temp <= 16'sb1000000000000000)
             mult_1_l1 = 16'sb1000000000000000;
         else
              mult_1_l1 = mult_1_l1_temp[15:0]; 
   	end
   	always_comb begin
         if((reset == 1) || (clear_acc_l1 == 1))
             output_data_l1 = 0;
         else begin
             if(output_data_1_l1 > 0)
         	    output_data_l1 = output_data_1_l1;
             else
         	    output_data_l1 = 0;
         end
         acc_1_l1 = output_data_1_l1 + mult_1_l1;
         if((output_data_1_l1[15] == 1) && (mult_1_l1[15] == 1) && (acc_1_l1[15] == 0))
             acc_1_l1 = 16'sb1000000000000000;
         else if((output_data_1_l1[15] == 0) && (mult_1_l1[15] == 0) && (acc_1_l1[15] == 1))
             acc_1_l1 = 16'sb0111111111111111;
         else
             acc_1_l1 = acc_1_l1;
   	end
endmodule

module control_1(clk, reset, input_valid, input_ready_l2, addr_x_l1, wr_en_x_l1, addr_w_1_l1, clear_acc_l1, en_acc_l1, input_ready, output_valid_l1, cl_x_l1, cl_addr_l1, en_mult_l1, multiplex_count_l1, state_l1);
   	input clk, reset, input_valid, input_ready_l2;
   	output logic output_valid_l1, input_ready;
   	logic [1:0] next_state_l1;
   	output logic [1:0] state_l1;
   	output logic clear_acc_l1, en_acc_l1;
   	output logic wr_en_x_l1;
   	logic [2:0]counter_x_l1;
   	logic [5:0]counter_w_l1;
   	output logic [1:0] addr_x_l1;
   	output logic [4:0] addr_w_1_l1;
   	output logic cl_x_l1;
   	output logic cl_addr_l1;
   	output logic en_mult_l1;
   	output logic multiplex_count_l1;
   	logic clear_multiplex_count_l1;
   	parameter [1:0] load_l1=2'b00, compute_l1=2'b01, stall_l1=2'b10, out_l1=2'b11;
   	always_ff @(posedge clk) begin
   	    if((reset  == 1) || (cl_addr_l1 == 1))begin
   	      	addr_x_l1 <= 0;
   	      	addr_w_1_l1 <= 0;
   	    end
   	    else if (cl_x_l1 == 1)
   	       	addr_x_l1 <= 0;
   	    else if((state_l1 == load_l1) && (input_valid == 1) && (counter_x_l1 < 4))
   	       	addr_x_l1 <= addr_x_l1 + 1;
   	    else if(state_l1 == compute_l1)begin
   	       	addr_x_l1 <= addr_x_l1 + 1;
   	       	addr_w_1_l1 <= addr_w_1_l1 + 1;
   	    end
   	end
   	always_ff @(posedge clk) begin
   	    if((reset  == 1) || (cl_addr_l1 == 1))begin
   	      	counter_x_l1 <= 0;
   	      	counter_w_l1 <= 0;
   	    end
   	    else if (cl_x_l1 == 1)
   	      	counter_x_l1 <= 0;
   	    else if((input_valid == 1) && (wr_en_x_l1 == 1))
   	      	counter_x_l1 <= counter_x_l1 + 1;
   	    else if(state_l1 == compute_l1)begin
   	      	counter_x_l1 <= counter_x_l1 + 1;
   	      	counter_w_l1 <= counter_w_l1 + 1;
   	    end
   	    if ((clear_multiplex_count_l1 == 1) || (reset == 1))
   	      	multiplex_count_l1 <= 0;
   	    else if((state_l1 == out_l1) && (input_ready_l2 == 1) && (output_valid_l1 == 1))
   	      	multiplex_count_l1 <= multiplex_count_l1 + 1;
   	end
   	always_comb begin
   	    if(state_l1 == load_l1)begin
   	      	en_mult_l1 = 0;
   	      	output_valid_l1 = 0;
   	      	en_acc_l1 = 0;
   	      	if((input_valid == 1))begin
   	      	    if(counter_x_l1 != 4)
   	      	        input_ready = 1;
   	      	    else
   	      	        input_ready = 0;
   	      	    if(counter_x_l1 < 4) begin
   	      	        wr_en_x_l1 = 1;
   	      	    end
   	      	    else begin
   	      	        wr_en_x_l1 = 0;
   	      	    end
   	      	end
   	      	else begin
   	      	    wr_en_x_l1 = 0;
   	      	    input_ready = 1;
   	      	end
   	    end
   	    else if (state_l1 == compute_l1)begin
   	      	if(counter_x_l1 > 0)
   	      	   en_mult_l1 = 1;
   	      	else
   	      	   en_mult_l1 = 0;
   	      	input_ready = 0;
   	      	wr_en_x_l1 = 0;
   	      	output_valid_l1 = 0;
   	      	if(counter_x_l1 > 0)
   	      	   	en_acc_l1 = 1;
   	      	else
   	      	   	en_acc_l1 = 0;
   	    end
   	    else if(state_l1 == out_l1)begin
   	      	input_ready = 0;
   	      	if(multiplex_count_l1 == 1)
   	      	   	output_valid_l1 = 0;
   	      	else
   	      	   	output_valid_l1 = 1;
   	      	en_acc_l1 = 0;
   	      	wr_en_x_l1 = 0;
   	      	en_mult_l1 = 0;
   	    end
   	    else begin
   	      	output_valid_l1 = 0;
   	      	input_ready = 0;
   	      	en_acc_l1 = 1;
   	      	wr_en_x_l1 = 0;
   	      	en_mult_l1 = 0;
   	    end
   	end
   	always_comb begin
   	    if(state_l1 == load_l1 )begin
   	      	if(counter_x_l1 < 4) begin
   	      	   	next_state_l1 = load_l1;
   	      	   	clear_acc_l1 = 0;
   	      	   	cl_addr_l1 = 0;
   	      	   	cl_x_l1 = 0;
   	      	   	clear_multiplex_count_l1 = 0;
   	        end
   	        else begin
   	      	   	next_state_l1 = compute_l1;
   	      	   	cl_addr_l1 = 1;
   	      	   	clear_acc_l1 = 1;
   	      	   	cl_x_l1 = 0;
   	      	   	clear_multiplex_count_l1 = 1;
   	        end
   	    end
   	    else if(state_l1 == compute_l1)begin
   	      	if(counter_x_l1 < 4) begin
   	      	   	next_state_l1 = compute_l1;
   	      	   	cl_addr_l1 = 0;
   	      	   	clear_acc_l1 = 0;
   	      	   	cl_x_l1 = 0;
   	      	   	clear_multiplex_count_l1 = 0;
   	        end
   	        else begin
   	      	   	next_state_l1 = stall_l1;
   	      	    cl_addr_l1 = 0;
   	      	   	cl_x_l1 = 1;
   	      	   	clear_acc_l1 = 0;
   	      	   	clear_multiplex_count_l1 = 0;
   	        end
   	    end
   	   	else if(state_l1 == stall_l1)begin
   	      	cl_x_l1 = 0;
   	      	clear_acc_l1 = 0;
   	      	cl_addr_l1 = 0;
   	      	next_state_l1 = out_l1;
   	      	clear_multiplex_count_l1 = 0;
   	    end
   	    else begin
   	      	if((multiplex_count_l1 < 1) && (counter_w_l1 < 32))begin
   	      	   	cl_x_l1 = 0;
   	      	   	clear_acc_l1 = 0;
   	      	   	cl_addr_l1 = 0;
   	      	   	next_state_l1 = out_l1;
   	      	   	clear_multiplex_count_l1 = 0;
   			end
   	      	else if((multiplex_count_l1 == 1) && (counter_w_l1 < 32))begin
   	      	   	clear_acc_l1 = 1;
   	      	   	next_state_l1 = compute_l1;
   	      	   	cl_addr_l1 = 0;
   	      	   	cl_x_l1 = 0;
   	      	   	clear_multiplex_count_l1 = 1;
   			end
   	      	else if((multiplex_count_l1 < 1) && (counter_w_l1 == 32))begin
   	      	   	cl_x_l1 = 0;
   	      	   	clear_acc_l1 = 0;
   	      	   	cl_addr_l1 = 0;
   	      	   	next_state_l1 = out_l1;
   	      	   	clear_multiplex_count_l1 = 0;
   			end
   	      	else begin
   	      	   	next_state_l1 = load_l1;
   	      	   	clear_acc_l1 = 1;
   	      	   	cl_addr_l1 = 1;
   	      	   	cl_x_l1 = 0;
   	      	   	clear_multiplex_count_l1 = 1;
   			end
   	    end
   	end
   	always_ff @(posedge clk) begin
   	    if (reset == 1)
   			state_l1 <= load_l1;
   	    else
   			state_l1 <= next_state_l1;
   	end
endmodule

module mem_x_l1(clk, data_in, data_out, addr, wr_en);
    parameter                   WIDTH=16, SIZE=4;
    localparam                  LOGSIZE=$clog2(SIZE);
    input [WIDTH-1:0]           data_in;
    output logic [WIDTH-1:0]    data_out;
    input [LOGSIZE-1:0]         addr;
    input                       clk, wr_en;

    logic [SIZE-1:0][WIDTH-1:0] mem;

    always_ff @(posedge clk) begin
        data_out <= mem[addr];
        if (wr_en)
            mem[addr] <= data_in;
    end
endmodule

module l1_fc_8_4_16_1_1_W_rom_1(clk, addr, z);
   input clk;
   input [4:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd8;
        1: z <= -16'd3;
        2: z <= -16'd6;
        3: z <= 16'd3;
        4: z <= -16'd7;
        5: z <= 16'd3;
        6: z <= 16'd4;
        7: z <= 16'd3;
        8: z <= -16'd3;
        9: z <= 16'd1;
        10: z <= 16'd7;
        11: z <= 16'd4;
        12: z <= 16'd4;
        13: z <= -16'd2;
        14: z <= 16'd1;
        15: z <= 16'd0;
        16: z <= 16'd4;
        17: z <= -16'd6;
        18: z <= 16'd1;
        19: z <= -16'd8;
        20: z <= 16'd4;
        21: z <= -16'd4;
        22: z <= 16'd7;
        23: z <= -16'd4;
        24: z <= -16'd2;
        25: z <= 16'd6;
        26: z <= -16'd4;
        27: z <= 16'd2;
        28: z <= 16'd5;
        29: z <= -16'd1;
        30: z <= 16'd3;
        31: z <= 16'd5;
      endcase
   end
endmodule


module l2_fc_12_8_16_1_1(clk, reset, output_valid_l1, input_ready_l2, output_data_l1, output_valid_l2, input_ready_l3, output_data_l2);
   input clk, reset, output_valid_l1, input_ready_l3;
   input signed [15:0] output_data_l1;
   output logic signed [15:0] output_data_l2;
   output logic output_valid_l2, input_ready_l2;
   fc_12_8_16_1_1_l2 fc_12_8_16_1_1_l2(clk, reset, output_valid_l1, input_ready_l2, output_data_l1, output_valid_l2, input_ready_l3, output_data_l2);
endmodule

module fc_12_8_16_1_1_l2(clk, reset, output_valid_l1, input_ready_l2, output_data_l1, output_valid_l2, input_ready_l3, output_data_l2);
   	input clk, reset, output_valid_l1, input_ready_l3;
   	input signed [15:0] output_data_l1;
   	output logic signed [15:0] output_data_l2;
   	output logic output_valid_l2, input_ready_l2;
   	logic clear_acc_l2, en_acc_l2;
   	logic [2:0] addr_x_l2;
   	logic [6:0] addr_w_1_l2;
   	logic wr_en_x_l2;
   	logic cl_x_l2;
   	logic cl_addr_l2;
   	logic en_mult_l2;
   	logic multiplex_count_l2;
   	logic [1:0] state_l2;
   	data_path_l2 data_path_l2(clk, reset, output_data_l1, addr_x_l2, wr_en_x_l2,addr_w_1_l2,	clear_acc_l2, en_acc_l2, output_data_l2, cl_x_l2, cl_addr_l2, en_mult_l2, multiplex_count_l2, state_l2);
   	control_l2 control_l2(clk, reset, output_valid_l1, input_ready_l3, addr_x_l2, wr_en_x_l2,addr_w_1_l2, clear_acc_l2, en_acc_l2, input_ready_l2, output_valid_l2, cl_x_l2, cl_addr_l2, en_mult_l2, multiplex_count_l2, state_l2);
endmodule

module data_path_l2(clk, reset, output_data_l1, addr_x_l2, wr_en_x_l2,addr_w_1_l2,clear_acc_l2, en_acc_l2, output_data_l2, cl_x_l2, cl_addr_l2, en_mult_l2, multiplex_count_l2, state_l2);
   	input clk, reset;
   	input signed [15:0] output_data_l1;
   	output logic signed [15:0] output_data_l2;
   	logic signed [15:0] output_data_1_l2;
   	input clear_acc_l2, en_acc_l2;
   	input wr_en_x_l2;
   	input [2:0] addr_x_l2;
   	input [6:0] addr_w_1_l2;
   	input cl_x_l2, cl_addr_l2;
   	input en_mult_l2;
   	logic signed [15:0] x_out_l2, w_out_1_l2;
   	logic signed [15:0] acc_1_l2;
   	logic signed [15:0] mult_1_l2;
   	logic signed [31:0] mult_1_l2_temp;
   	input multiplex_count_l2;
   	input [1:0] state_l2;
   	parameter [1:0] load_l2=2'b00, compute_l2=2'b01, stall_l2=2'b10, out_l2=2'b11;
   	l2_fc_12_8_16_1_1_W_rom_1 mem_w_1_l2(clk, addr_w_1_l2, w_out_1_l2);
   	mem_x_l2 mem_x_l2(clk, output_data_l1, x_out_l2, addr_x_l2, wr_en_x_l2);

   	always_ff @(posedge clk)begin
   		 if((reset == 1) || (clear_acc_l2 == 1))begin
   			 output_data_1_l2 <= 0;
   		 end
   		 else if(en_acc_l2 == 1)begin
   			 output_data_1_l2 <= acc_1_l2;
   		 end
   	end
   	always_ff @(posedge clk)begin
   		 if(en_mult_l2 == 1) begin
   			 mult_1_l2_temp <= w_out_1_l2 * x_out_l2;
   		 end
   		 else begin
   			 mult_1_l2_temp <= 0;
   		 end
   	end
   	always_comb begin
         if(mult_1_l2_temp >= 16'sb0111111111111111)
             mult_1_l2 = 16'sb0111111111111111;
         else if(mult_1_l2_temp <= 16'sb1000000000000000)
             mult_1_l2 = 16'sb1000000000000000;
         else
              mult_1_l2 = mult_1_l2_temp[15:0]; 
   	end
   	always_comb begin
         if((reset == 1) || (clear_acc_l2 == 1))
             output_data_l2 = 0;
         else begin
             if(output_data_1_l2 > 0)
         	    output_data_l2 = output_data_1_l2;
             else
         	    output_data_l2 = 0;
         end
         acc_1_l2 = output_data_1_l2 + mult_1_l2;
         if((output_data_1_l2[15] == 1) && (mult_1_l2[15] == 1) && (acc_1_l2[15] == 0))
             acc_1_l2 = 16'sb1000000000000000;
         else if((output_data_1_l2[15] == 0) && (mult_1_l2[15] == 0) && (acc_1_l2[15] == 1))
             acc_1_l2 = 16'sb0111111111111111;
         else
             acc_1_l2 = acc_1_l2;
   	end
endmodule

module control_l2(clk, reset, output_valid_l1, input_ready_l3, addr_x_l2, wr_en_x_l2, addr_w_1_l2, clear_acc_l2, en_acc_l2, input_ready_l2, output_valid_l2, cl_x_l2, cl_addr_l2, en_mult_l2, multiplex_count_l2, state_l2);
   	input clk, reset, output_valid_l1, input_ready_l3;
   	output logic output_valid_l2, input_ready_l2;
   	logic [1:0] next_state_l2;
   	output logic [1:0] state_l2;
   	output logic clear_acc_l2, en_acc_l2;
   	output logic wr_en_x_l2;
   	logic [3:0]counter_x_l2;
   	logic [6:0]counter_w_l2;
   	output logic [2:0] addr_x_l2;
   	output logic [6:0] addr_w_1_l2;
   	output logic cl_x_l2;
   	output logic cl_addr_l2;
   	output logic en_mult_l2;
   	output logic multiplex_count_l2;
   	logic clear_multiplex_count_l2;
   	parameter [1:0] load_l2=2'b00, compute_l2=2'b01, stall_l2=2'b10, out_l2=2'b11;
   	always_ff @(posedge clk) begin
   	    if((reset  == 1) || (cl_addr_l2 == 1))begin
   	      	addr_x_l2 <= 0;
   	      	addr_w_1_l2 <= 0;
   	    end
   	    else if (cl_x_l2 == 1)
   	       	addr_x_l2 <= 0;
   	    else if((state_l2 == load_l2) && (output_valid_l1 == 1) && (counter_x_l2 < 8))
   	       	addr_x_l2 <= addr_x_l2 + 1;
   	    else if(state_l2 == compute_l2)begin
   	       	addr_x_l2 <= addr_x_l2 + 1;
   	       	addr_w_1_l2 <= addr_w_1_l2 + 1;
   	    end
   	end
   	always_ff @(posedge clk) begin
   	    if((reset  == 1) || (cl_addr_l2 == 1))begin
   	      	counter_x_l2 <= 0;
   	      	counter_w_l2 <= 0;
   	    end
   	    else if (cl_x_l2 == 1)
   	      	counter_x_l2 <= 0;
   	    else if((output_valid_l1 == 1) && (wr_en_x_l2 == 1))
   	      	counter_x_l2 <= counter_x_l2 + 1;
   	    else if(state_l2 == compute_l2)begin
   	      	counter_x_l2 <= counter_x_l2 + 1;
   	      	counter_w_l2 <= counter_w_l2 + 1;
   	    end
   	    if ((clear_multiplex_count_l2 == 1) || (reset == 1))
   	      	multiplex_count_l2 <= 0;
   	    else if((state_l2 == out_l2) && (input_ready_l3 == 1) && (output_valid_l2 == 1))
   	      	multiplex_count_l2 <= multiplex_count_l2 + 1;
   	end
   	always_comb begin
   	    if(state_l2 == load_l2)begin
   	      	en_mult_l2 = 0;
   	      	output_valid_l2 = 0;
   	      	en_acc_l2 = 0;
   	      	if((output_valid_l1 == 1))begin
   	      	    if(counter_x_l2 != 8)
   	      	        input_ready_l2 = 1;
   	      	    else
   	      	        input_ready_l2 = 0;
   	      	    if(counter_x_l2 < 8) begin
   	      	        wr_en_x_l2 = 1;
   	      	    end
   	      	    else begin
   	      	        wr_en_x_l2 = 0;
   	      	    end
   	      	end
   	      	else begin
   	      	    wr_en_x_l2 = 0;
   	      	    input_ready_l2 = 1;
   	      	end
   	    end
   	    else if (state_l2 == compute_l2)begin
   	      	if(counter_x_l2 > 0)
   	      	   en_mult_l2 = 1;
   	      	else
   	      	   en_mult_l2 = 0;
   	      	input_ready_l2 = 0;
   	      	wr_en_x_l2 = 0;
   	      	output_valid_l2 = 0;
   	      	if(counter_x_l2 > 0)
   	      	   	en_acc_l2 = 1;
   	      	else
   	      	   	en_acc_l2 = 0;
   	    end
   	    else if(state_l2 == out_l2)begin
   	      	input_ready_l2 = 0;
   	      	if(multiplex_count_l2 == 1)
   	      	   	output_valid_l2 = 0;
   	      	else
   	      	   	output_valid_l2 = 1;
   	      	en_acc_l2 = 0;
   	      	wr_en_x_l2 = 0;
   	      	en_mult_l2 = 0;
   	    end
   	    else begin
   	      	output_valid_l2 = 0;
   	      	input_ready_l2 = 0;
   	      	en_acc_l2 = 1;
   	      	wr_en_x_l2 = 0;
   	      	en_mult_l2 = 0;
   	    end
   	end
   	always_comb begin
   	    if(state_l2 == load_l2 )begin
   	      	if(counter_x_l2 < 8) begin
   	      	   	next_state_l2 = load_l2;
   	      	   	clear_acc_l2 = 0;
   	      	   	cl_addr_l2 = 0;
   	      	   	cl_x_l2 = 0;
   	      	   	clear_multiplex_count_l2 = 0;
   	        end
   	        else begin
   	      	   	next_state_l2 = compute_l2;
   	      	   	cl_addr_l2 = 1;
   	      	   	clear_acc_l2 = 1;
   	      	   	cl_x_l2 = 0;
   	      	   	clear_multiplex_count_l2 = 1;
   	        end
   	    end
   	    else if(state_l2 == compute_l2)begin
   	      	if(counter_x_l2 < 8) begin
   	      	   	next_state_l2 = compute_l2;
   	      	   	cl_addr_l2 = 0;
   	      	   	clear_acc_l2 = 0;
   	      	   	cl_x_l2 = 0;
   	      	   	clear_multiplex_count_l2 = 0;
   	        end
   	        else begin
   	      	   	next_state_l2 = stall_l2;
   	      	    cl_addr_l2 = 0;
   	      	   	cl_x_l2 = 1;
   	      	   	clear_acc_l2 = 0;
   	      	   	clear_multiplex_count_l2 = 0;
   	        end
   	    end
   	   	else if(state_l2 == stall_l2)begin
   	      	cl_x_l2 = 0;
   	      	clear_acc_l2 = 0;
   	      	cl_addr_l2 = 0;
   	      	next_state_l2 = out_l2;
   	      	clear_multiplex_count_l2 = 0;
   	    end
   	    else begin
   	      	if((multiplex_count_l2 < 1) && (counter_w_l2 < 96))begin
   	      	   	cl_x_l2 = 0;
   	      	   	clear_acc_l2 = 0;
   	      	   	cl_addr_l2 = 0;
   	      	   	next_state_l2 = out_l2;
   	      	   	clear_multiplex_count_l2 = 0;
   			end
   	      	else if((multiplex_count_l2 == 1) && (counter_w_l2 < 96))begin
   	      	   	clear_acc_l2 = 1;
   	      	   	next_state_l2 = compute_l2;
   	      	   	cl_addr_l2 = 0;
   	      	   	cl_x_l2 = 0;
   	      	   	clear_multiplex_count_l2 = 1;
   			end
   	      	else if((multiplex_count_l2 < 1) && (counter_w_l2 == 96))begin
   	      	   	cl_x_l2 = 0;
   	      	   	clear_acc_l2 = 0;
   	      	   	cl_addr_l2 = 0;
   	      	   	next_state_l2 = out_l2;
   	      	   	clear_multiplex_count_l2 = 0;
   			end
   	      	else begin
   	      	   	next_state_l2 = load_l2;
   	      	   	clear_acc_l2 = 1;
   	      	   	cl_addr_l2 = 1;
   	      	   	cl_x_l2 = 0;
   	      	   	clear_multiplex_count_l2 = 1;
   			end
   	    end
   	end
   	always_ff @(posedge clk) begin
   	    if (reset == 1)
   			state_l2 <= load_l2;
   	    else
   			state_l2 <= next_state_l2;
   	end
endmodule

module mem_x_l2(clk, data_in, data_out, addr, wr_en);
    parameter                   WIDTH=16, SIZE=8;
    localparam                  LOGSIZE=$clog2(SIZE);
    input [WIDTH-1:0]           data_in;
    output logic [WIDTH-1:0]    data_out;
    input [LOGSIZE-1:0]         addr;
    input                       clk, wr_en;

    logic [SIZE-1:0][WIDTH-1:0] mem;

    always_ff @(posedge clk) begin
        data_out <= mem[addr];
        if (wr_en)
            mem[addr] <= data_in;
    end
endmodule

module l2_fc_12_8_16_1_1_W_rom_1(clk, addr, z);
   input clk;
   input [6:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= 16'd4;
        1: z <= 16'd5;
        2: z <= 16'd0;
        3: z <= 16'd6;
        4: z <= 16'd1;
        5: z <= -16'd4;
        6: z <= 16'd1;
        7: z <= 16'd6;
        8: z <= 16'd6;
        9: z <= 16'd0;
        10: z <= 16'd2;
        11: z <= 16'd2;
        12: z <= 16'd6;
        13: z <= -16'd4;
        14: z <= -16'd6;
        15: z <= 16'd2;
        16: z <= -16'd2;
        17: z <= 16'd3;
        18: z <= 16'd2;
        19: z <= -16'd6;
        20: z <= -16'd8;
        21: z <= 16'd1;
        22: z <= -16'd2;
        23: z <= -16'd2;
        24: z <= -16'd1;
        25: z <= 16'd2;
        26: z <= -16'd8;
        27: z <= -16'd4;
        28: z <= -16'd7;
        29: z <= 16'd3;
        30: z <= -16'd7;
        31: z <= 16'd6;
        32: z <= 16'd1;
        33: z <= 16'd1;
        34: z <= 16'd4;
        35: z <= -16'd6;
        36: z <= 16'd5;
        37: z <= -16'd3;
        38: z <= -16'd8;
        39: z <= 16'd3;
        40: z <= 16'd5;
        41: z <= 16'd2;
        42: z <= -16'd3;
        43: z <= 16'd3;
        44: z <= 16'd6;
        45: z <= -16'd1;
        46: z <= -16'd3;
        47: z <= -16'd3;
        48: z <= -16'd5;
        49: z <= 16'd7;
        50: z <= -16'd1;
        51: z <= -16'd5;
        52: z <= 16'd0;
        53: z <= 16'd6;
        54: z <= 16'd1;
        55: z <= 16'd7;
        56: z <= 16'd0;
        57: z <= 16'd2;
        58: z <= -16'd5;
        59: z <= 16'd2;
        60: z <= -16'd3;
        61: z <= -16'd4;
        62: z <= 16'd0;
        63: z <= 16'd6;
        64: z <= 16'd5;
        65: z <= -16'd4;
        66: z <= -16'd8;
        67: z <= 16'd2;
        68: z <= 16'd1;
        69: z <= -16'd8;
        70: z <= -16'd2;
        71: z <= -16'd2;
        72: z <= 16'd3;
        73: z <= 16'd3;
        74: z <= -16'd7;
        75: z <= 16'd1;
        76: z <= -16'd5;
        77: z <= -16'd2;
        78: z <= 16'd6;
        79: z <= -16'd2;
        80: z <= -16'd3;
        81: z <= -16'd2;
        82: z <= 16'd1;
        83: z <= 16'd5;
        84: z <= -16'd4;
        85: z <= -16'd6;
        86: z <= 16'd4;
        87: z <= 16'd4;
        88: z <= 16'd4;
        89: z <= 16'd7;
        90: z <= -16'd2;
        91: z <= -16'd6;
        92: z <= -16'd5;
        93: z <= 16'd6;
        94: z <= -16'd8;
        95: z <= -16'd8;
      endcase
   end
endmodule


module l3_fc_16_12_16_1_1(clk, reset, output_valid_l2, input_ready_l3, output_data_l2, output_valid, output_ready, output_data);
   input clk, reset, output_valid_l2, output_ready;
   input signed [15:0] output_data_l2;
   output logic signed [15:0] output_data;
   output logic output_valid, input_ready_l3;
   fc_16_12_16_1_1_l3 fc_16_12_16_1_1_l3(clk, reset, output_valid_l2, input_ready_l3, output_data_l2, output_valid, output_ready, output_data);
endmodule

module fc_16_12_16_1_1_l3(clk, reset, output_valid_l2, input_ready_l3, output_data_l2, output_valid, output_ready, output_data);
   	input clk, reset, output_valid_l2, output_ready;
   	input signed [15:0] output_data_l2;
   	output logic signed [15:0] output_data;
   	output logic output_valid, input_ready_l3;
   	logic clear_acc_l3, en_acc_l3;
   	logic [3:0] addr_x_l3;
   	logic [7:0] addr_w_1_l3;
   	logic wr_en_x_l3;
   	logic cl_x_l3;
   	logic cl_addr_l3;
   	logic en_mult_l3;
   	logic multiplex_count_l3;
   	logic [1:0] state_l3;
   	data_path_l3 data_path_l3(clk, reset, output_data_l2, addr_x_l3, wr_en_x_l3,addr_w_1_l3,	clear_acc_l3, en_acc_l3, output_data, cl_x_l3, cl_addr_l3, en_mult_l3, multiplex_count_l3, state_l3);
   	control_l3 control_l3(clk, reset, output_valid_l2, output_ready, addr_x_l3, wr_en_x_l3,addr_w_1_l3, clear_acc_l3, en_acc_l3, input_ready_l3, output_valid, cl_x_l3, cl_addr_l3, en_mult_l3, multiplex_count_l3, state_l3);
endmodule

module data_path_l3(clk, reset, output_data_l2, addr_x_l3, wr_en_x_l3,addr_w_1_l3, clear_acc_l3, en_acc_l3, output_data, cl_x_l3, cl_addr_l3, en_mult_l3, multiplex_count_l3, state_l3);
   	input clk, reset;
   	input signed [15:0] output_data_l2;
   	output logic signed [15:0] output_data;
   	logic signed [15:0] output_data_1_l3;
   	input clear_acc_l3, en_acc_l3;
   	input wr_en_x_l3;
   	input [3:0] addr_x_l3;
   	input [7:0] addr_w_1_l3;
   	input cl_x_l3, cl_addr_l3;
   	input en_mult_l3;
   	logic signed [15:0] x_out_l3, w_out_1_l3;
   	logic signed [15:0] acc_1_l3;
   	logic signed [15:0] mult_1_l3;
   	logic signed [31:0] mult_1_l3_temp;
   	input multiplex_count_l3;
   	input [1:0] state_l3;
   	parameter [1:0] load=2'b00, compute=2'b01, stall=2'b10, out=2'b11;
   	l3_fc_16_12_16_1_1_W_rom_1 mem_w_1_l3(clk, addr_w_1_l3, w_out_1_l3);
   	mem_x_l3 mem_x_l3(clk, output_data_l2, x_out_l3, addr_x_l3, wr_en_x_l3);

   	always_ff @(posedge clk)begin
   		 if((reset == 1) || (clear_acc_l3 == 1))begin
   			 output_data_1_l3 <= 0;
   		 end
   		 else if(en_acc_l3 == 1)begin
   			 output_data_1_l3 <= acc_1_l3;
   		 end
   	end
   	always_ff @(posedge clk)begin
   		 if(en_mult_l3 == 1) begin
   			 mult_1_l3_temp <= w_out_1_l3 * x_out_l3;
   		 end
   		 else begin
   			 mult_1_l3_temp <= 0;
   		 end
   	end
   	always_comb begin
         if(mult_1_l3_temp >= 16'sb0111111111111111)
             mult_1_l3 = 16'sb0111111111111111;
         else if(mult_1_l3_temp <= 16'sb1000000000000000)
             mult_1_l3 = 16'sb1000000000000000;
         else
              mult_1_l3 = mult_1_l3_temp[15:0]; 
   	end
   	always_comb begin
         if((reset == 1) || (clear_acc_l3 == 1))
             output_data = 0;
         else begin
             if(output_data_1_l3 > 0)
         	    output_data = output_data_1_l3;
             else
         	    output_data = 0;
         end
         acc_1_l3 = output_data_1_l3 + mult_1_l3;
         if((output_data_1_l3[15] == 1) && (mult_1_l3[15] == 1) && (acc_1_l3[15] == 0))
             acc_1_l3 = 16'sb1000000000000000;
         else if((output_data_1_l3[15] == 0) && (mult_1_l3[15] == 0) && (acc_1_l3[15] == 1))
             acc_1_l3 = 16'sb0111111111111111;
         else
             acc_1_l3 = acc_1_l3;
   	end
endmodule

module control_l3(clk, reset, output_valid_l2, output_ready, addr_x_l3, wr_en_x_l3, addr_w_1_l3, clear_acc_l3, en_acc_l3, input_ready_l3, output_valid, cl_x_l3, cl_addr_l3, en_mult_l3, multiplex_count_l3, state_l3);
   	input clk, reset, output_valid_l2, output_ready;
   	output logic output_valid, input_ready_l3;
   	logic [1:0] next_state;
   	output logic [1:0] state_l3;
   	output logic clear_acc_l3, en_acc_l3;
   	output logic wr_en_x_l3;
   	logic [3:0]counter_x;
   	logic [7:0]counter_w;
   	output logic [3:0] addr_x_l3;
   	output logic [7:0] addr_w_1_l3;
   	output logic cl_x_l3;
   	output logic cl_addr_l3;
   	output logic en_mult_l3;
   	output logic multiplex_count_l3;
   	logic clear_multiplex_count;
   	parameter [1:0] load=2'b00, compute=2'b01, stall=2'b10, out=2'b11;
   	always_ff @(posedge clk) begin
   	    if((reset  == 1) || (cl_addr_l3 == 1))begin
   	      	addr_x_l3 <= 0;
   	      	addr_w_1_l3 <= 0;
   	    end
   	    else if (cl_x_l3 == 1)
   	       	addr_x_l3 <= 0;
   	    else if((state_l3 == load) && (output_valid_l2 == 1) && (counter_x < 12))
   	       	addr_x_l3 <= addr_x_l3 + 1;
   	    else if(state_l3 == compute)begin
   	       	addr_x_l3 <= addr_x_l3 + 1;
   	       	addr_w_1_l3 <= addr_w_1_l3 + 1;
   	    end
   	end
   	always_ff @(posedge clk) begin
   	    if((reset  == 1) || (cl_addr_l3 == 1))begin
   	      	counter_x <= 0;
   	      	counter_w <= 0;
   	    end
   	    else if (cl_x_l3 == 1)
   	      	counter_x <= 0;
   	    else if((output_valid_l2 == 1) && (wr_en_x_l3 == 1))
   	      	counter_x <= counter_x + 1;
   	    else if(state_l3 == compute)begin
   	      	counter_x <= counter_x + 1;
   	      	counter_w <= counter_w + 1;
   	    end
   	    if ((clear_multiplex_count == 1) || (reset == 1))
   	      	multiplex_count_l3 <= 0;
   	    else if((state_l3 == out) && (output_ready == 1) && (output_valid == 1))
   	      	multiplex_count_l3 <= multiplex_count_l3 + 1;
   	end
   	always_comb begin
   	    if(state_l3 == load)begin
   	      	en_mult_l3 = 0;
   	      	output_valid = 0;
   	      	en_acc_l3 = 0;
   	      	if((output_valid_l2 == 1))begin
   	      	    if(counter_x != 12)
   	      	        input_ready_l3 = 1;
   	      	    else
   	      	        input_ready_l3 = 0;
   	      	    if(counter_x < 12) begin
   	      	        wr_en_x_l3 = 1;
   	      	    end
   	      	    else begin
   	      	        wr_en_x_l3 = 0;
   	      	    end
   	      	end
   	      	else begin
   	      	    wr_en_x_l3 = 0;
   	      	    input_ready_l3 = 1;
   	      	end
   	    end
   	    else if (state_l3 == compute)begin
   	      	if(counter_x > 0)
   	      	   en_mult_l3 = 1;
   	      	else
   	      	   en_mult_l3 = 0;
   	      	input_ready_l3 = 0;
   	      	wr_en_x_l3 = 0;
   	      	output_valid = 0;
   	      	if(counter_x > 0)
   	      	   	en_acc_l3 = 1;
   	      	else
   	      	   	en_acc_l3 = 0;
   	    end
   	    else if(state_l3 == out)begin
   	      	input_ready_l3 = 0;
   	      	if(multiplex_count_l3 == 1)
   	      	   	output_valid = 0;
   	      	else
   	      	   	output_valid = 1;
   	      	en_acc_l3 = 0;
   	      	wr_en_x_l3 = 0;
   	      	en_mult_l3 = 0;
   	    end
   	    else begin
   	      	output_valid = 0;
   	      	input_ready_l3 = 0;
   	      	en_acc_l3 = 1;
   	      	wr_en_x_l3 = 0;
   	      	en_mult_l3 = 0;
   	    end
   	end
   	always_comb begin
   	    if(state_l3 == load )begin
   	      	if(counter_x < 12) begin
   	      	   	next_state = load;
   	      	   	clear_acc_l3 = 0;
   	      	   	cl_addr_l3 = 0;
   	      	   	cl_x_l3 = 0;
   	      	   	clear_multiplex_count = 0;
   	        end
   	        else begin
   	      	   	next_state = compute;
   	      	   	cl_addr_l3 = 1;
   	      	   	clear_acc_l3 = 1;
   	      	   	cl_x_l3 = 0;
   	      	   	clear_multiplex_count = 1;
   	        end
   	    end
   	    else if(state_l3 == compute)begin
   	      	if(counter_x < 12) begin
   	      	   	next_state = compute;
   	      	   	cl_addr_l3 = 0;
   	      	   	clear_acc_l3 = 0;
   	      	   	cl_x_l3 = 0;
   	      	   	clear_multiplex_count = 0;
   	        end
   	        else begin
   	      	   	next_state = stall;
   	      	    cl_addr_l3 = 0;
   	      	   	cl_x_l3 = 1;
   	      	   	clear_acc_l3 = 0;
   	      	   	clear_multiplex_count = 0;
   	        end
   	    end
   	   	else if(state_l3 == stall)begin
   	      	cl_x_l3 = 0;
   	      	clear_acc_l3 = 0;
   	      	cl_addr_l3 = 0;
   	      	next_state = out;
   	      	clear_multiplex_count = 0;
   	    end
   	    else begin
   	      	if((multiplex_count_l3 < 1) && (counter_w < 192))begin
   	      	   	cl_x_l3 = 0;
   	      	   	clear_acc_l3 = 0;
   	      	   	cl_addr_l3 = 0;
   	      	   	next_state = out;
   	      	   	clear_multiplex_count = 0;
   			end
   	      	else if((multiplex_count_l3 == 1) && (counter_w < 192))begin
   	      	   	clear_acc_l3 = 1;
   	      	   	next_state = compute;
   	      	   	cl_addr_l3 = 0;
   	      	   	cl_x_l3 = 0;
   	      	   	clear_multiplex_count = 1;
   			end
   	      	else if((multiplex_count_l3 < 1) && (counter_w == 192))begin
   	      	   	cl_x_l3 = 0;
   	      	   	clear_acc_l3 = 0;
   	      	   	cl_addr_l3 = 0;
   	      	   	next_state = out;
   	      	   	clear_multiplex_count = 0;
   			end
   	      	else begin
   	      	   	next_state = load;
   	      	   	clear_acc_l3 = 1;
   	      	   	cl_addr_l3 = 1;
   	      	   	cl_x_l3 = 0;
   	      	   	clear_multiplex_count = 1;
   			end
   	    end
   	end
   	always_ff @(posedge clk) begin
   	    if (reset == 1)
   			state_l3 <= load;
   	    else
   			state_l3 <= next_state;
   	end
endmodule

module mem_x_l3(clk, data_in, data_out, addr, wr_en);
    parameter                   WIDTH=16, SIZE=12;
    localparam                  LOGSIZE=$clog2(SIZE);
    input [WIDTH-1:0]           data_in;
    output logic [WIDTH-1:0]    data_out;
    input [LOGSIZE-1:0]         addr;
    input                       clk, wr_en;

    logic [SIZE-1:0][WIDTH-1:0] mem;

    always_ff @(posedge clk) begin
        data_out <= mem[addr];
        if (wr_en)
            mem[addr] <= data_in;
    end
endmodule

module l3_fc_16_12_16_1_1_W_rom_1(clk, addr, z);
   input clk;
   input [7:0] addr;
   output logic signed [15:0] z;
   always_ff @(posedge clk) begin
      case(addr)
        0: z <= -16'd6;
        1: z <= -16'd7;
        2: z <= 16'd2;
        3: z <= 16'd3;
        4: z <= -16'd7;
        5: z <= -16'd8;
        6: z <= -16'd7;
        7: z <= 16'd4;
        8: z <= 16'd4;
        9: z <= -16'd6;
        10: z <= -16'd2;
        11: z <= 16'd7;
        12: z <= 16'd0;
        13: z <= -16'd4;
        14: z <= -16'd3;
        15: z <= 16'd5;
        16: z <= 16'd2;
        17: z <= 16'd6;
        18: z <= 16'd2;
        19: z <= 16'd6;
        20: z <= -16'd8;
        21: z <= -16'd2;
        22: z <= 16'd3;
        23: z <= 16'd5;
        24: z <= -16'd3;
        25: z <= -16'd7;
        26: z <= 16'd7;
        27: z <= 16'd0;
        28: z <= -16'd8;
        29: z <= 16'd7;
        30: z <= 16'd0;
        31: z <= -16'd6;
        32: z <= -16'd8;
        33: z <= -16'd5;
        34: z <= 16'd6;
        35: z <= -16'd6;
        36: z <= -16'd5;
        37: z <= 16'd7;
        38: z <= 16'd6;
        39: z <= 16'd7;
        40: z <= -16'd6;
        41: z <= -16'd4;
        42: z <= 16'd6;
        43: z <= 16'd2;
        44: z <= 16'd1;
        45: z <= -16'd5;
        46: z <= 16'd0;
        47: z <= -16'd5;
        48: z <= -16'd7;
        49: z <= -16'd6;
        50: z <= -16'd6;
        51: z <= -16'd6;
        52: z <= 16'd1;
        53: z <= 16'd5;
        54: z <= 16'd7;
        55: z <= 16'd6;
        56: z <= 16'd6;
        57: z <= 16'd6;
        58: z <= -16'd1;
        59: z <= 16'd6;
        60: z <= 16'd5;
        61: z <= 16'd7;
        62: z <= -16'd7;
        63: z <= 16'd6;
        64: z <= -16'd6;
        65: z <= 16'd7;
        66: z <= -16'd8;
        67: z <= -16'd2;
        68: z <= 16'd6;
        69: z <= 16'd6;
        70: z <= -16'd3;
        71: z <= -16'd8;
        72: z <= -16'd5;
        73: z <= -16'd4;
        74: z <= 16'd3;
        75: z <= 16'd4;
        76: z <= -16'd1;
        77: z <= -16'd5;
        78: z <= 16'd7;
        79: z <= 16'd1;
        80: z <= -16'd3;
        81: z <= -16'd7;
        82: z <= 16'd3;
        83: z <= 16'd6;
        84: z <= 16'd6;
        85: z <= 16'd2;
        86: z <= 16'd5;
        87: z <= 16'd5;
        88: z <= 16'd0;
        89: z <= -16'd4;
        90: z <= 16'd3;
        91: z <= -16'd3;
        92: z <= -16'd5;
        93: z <= 16'd4;
        94: z <= -16'd5;
        95: z <= -16'd2;
        96: z <= 16'd3;
        97: z <= -16'd5;
        98: z <= 16'd4;
        99: z <= 16'd2;
        100: z <= -16'd6;
        101: z <= -16'd7;
        102: z <= 16'd2;
        103: z <= -16'd3;
        104: z <= -16'd3;
        105: z <= -16'd3;
        106: z <= -16'd7;
        107: z <= 16'd5;
        108: z <= 16'd0;
        109: z <= -16'd8;
        110: z <= -16'd2;
        111: z <= 16'd6;
        112: z <= -16'd6;
        113: z <= -16'd7;
        114: z <= 16'd4;
        115: z <= -16'd8;
        116: z <= 16'd3;
        117: z <= 16'd1;
        118: z <= 16'd5;
        119: z <= -16'd5;
        120: z <= 16'd5;
        121: z <= 16'd1;
        122: z <= 16'd0;
        123: z <= -16'd7;
        124: z <= -16'd3;
        125: z <= 16'd4;
        126: z <= -16'd1;
        127: z <= -16'd7;
        128: z <= 16'd7;
        129: z <= -16'd5;
        130: z <= 16'd3;
        131: z <= -16'd7;
        132: z <= -16'd4;
        133: z <= -16'd3;
        134: z <= -16'd2;
        135: z <= 16'd2;
        136: z <= 16'd3;
        137: z <= -16'd1;
        138: z <= -16'd1;
        139: z <= -16'd5;
        140: z <= 16'd0;
        141: z <= 16'd5;
        142: z <= -16'd7;
        143: z <= 16'd2;
        144: z <= 16'd6;
        145: z <= 16'd6;
        146: z <= 16'd2;
        147: z <= 16'd1;
        148: z <= -16'd1;
        149: z <= 16'd0;
        150: z <= 16'd4;
        151: z <= -16'd3;
        152: z <= -16'd7;
        153: z <= -16'd4;
        154: z <= -16'd2;
        155: z <= -16'd2;
        156: z <= -16'd8;
        157: z <= 16'd5;
        158: z <= -16'd1;
        159: z <= -16'd8;
        160: z <= -16'd8;
        161: z <= -16'd6;
        162: z <= -16'd7;
        163: z <= -16'd4;
        164: z <= 16'd0;
        165: z <= 16'd0;
        166: z <= 16'd6;
        167: z <= -16'd5;
        168: z <= 16'd7;
        169: z <= -16'd3;
        170: z <= -16'd2;
        171: z <= -16'd1;
        172: z <= -16'd6;
        173: z <= 16'd0;
        174: z <= -16'd7;
        175: z <= -16'd8;
        176: z <= -16'd2;
        177: z <= 16'd4;
        178: z <= 16'd1;
        179: z <= 16'd5;
        180: z <= -16'd4;
        181: z <= -16'd3;
        182: z <= -16'd6;
        183: z <= -16'd3;
        184: z <= 16'd2;
        185: z <= 16'd0;
        186: z <= 16'd3;
        187: z <= 16'd2;
        188: z <= -16'd3;
        189: z <= -16'd5;
        190: z <= 16'd2;
        191: z <= -16'd3;
      endcase
   end
endmodule


