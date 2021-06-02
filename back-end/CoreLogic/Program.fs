open System
open Parser
open FParsec
open CommonTypes
open Simulator

let program1 = @"
module UART_TX(
    input wire clk,
    input wire resetn,
    input wire tx_start,        
    input wire b_tick,          
    input wire [8:0] d_in,      
    output reg tx_done,         
    output wire tx           
    ); 
    
    reg [1:0] current_state;
    reg [1:0] next_state;
    reg [3:0] b_reg;          
    reg [3:0] b_next;
    reg [3:0] count_reg;      
    reg [3:0] count_next;
    reg [8:0] data_reg;       
    reg [8:0] data_next;
    reg tx_reg;              
    reg tx_next;
    
    always @(posedge clk, negedge resetn) begin
        if(!resetn) begin
            current_state <= 1;
            b_reg <= 0;
            count_reg <= 0;
            data_reg <= 0;
            tx_reg <= 1'b1;
        end else begin
            current_state <= next_state;
            b_reg <= b_next;
            count_reg <= count_next;
            data_reg <= data_next;
            tx_reg <= tx_next;
        end
    end

    always @* begin
        next_state = current_state;
        tx_done = 1'b0;
        b_next = b_reg;
        count_next = count_reg;
        data_next = data_reg;
        tx_next = tx_reg;

        case(current_state)
            1: begin
                tx_next = 1'b1;
                if(tx_start) begin
                    next_state = 2;
                    b_next = 0;
                    data_next = d_in;
                end
            end

            2: begin
                tx_next = 1'b0;
                if(b_tick)
                    if(b_reg==15)
                        begin
                            next_state = 3;
                            b_next = 0;
                            count_next = 0;
                        end
                    else
                        b_next = b_reg + 1;
            end

            3: begin
                tx_next = data_reg[0];

                if(b_tick)
                    if(b_reg == 15) begin
                        b_next = 0;
                        data_next = data_reg >> 1;
                        if(count_reg == 8)    
                            next_state = 4;
                        else
                            count_next = count_reg + 1;
                    end else
                        b_next = b_reg + 1;
            end

            4: begin
                tx_next = 1'b1;
                if(b_tick)
                    if(b_reg == 15) begin
                        next_state = 1;
                        tx_done = 1'b1;
                    end else
                        b_next = b_reg + 1;
            end
        endcase
    end
    
    assign tx = tx_reg;
    
endmodule
"

let program2 = @"
module UART_TX(
    clk,
    resetn,
    tx_start,        
    b_tick,          
    d_in,      
    tx_done,         
    tx              
    );
    input wire clk;
    input wire resetn;
    input wire tx_start;        
    input wire b_tick;          
    input wire [8:0] d_in;      
    output reg tx_done;         
    output wire tx;  
    
    reg [1:0] current_state;
    reg [1:0] next_state;
    reg [3:0] b_reg;          
    reg [3:0] b_next;
    reg [3:0] count_reg;      
    reg [3:0] count_next;
    reg [8:0] data_reg;       
    reg [8:0] data_next;
    reg tx_reg;              
    reg tx_next;
    
    always @(posedge clk, negedge resetn) begin
        if(!resetn) begin
            current_state <= 1;
            b_reg <= 0;
            count_reg <= 0;
            data_reg <= 0;
            tx_reg <= 1'b1;
        end else begin
            current_state <= next_state;
            b_reg <= b_next;
            count_reg <= count_next;
            data_reg <= data_next;
            tx_reg <= tx_next;
        end
    end

    always @* begin
        next_state = current_state;
        tx_done = 1'b0;
        b_next = b_reg;
        count_next = count_reg;
        data_next = data_reg;
        tx_next = tx_reg;

        case(current_state)
            1: begin
                tx_next = 1'b1;
                if(tx_start) begin
                    next_state = 2;
                    b_next = 0;
                    data_next = d_in;
                end
            end

            2: begin
                tx_next = 1'b0;
                if(b_tick)
                    if(b_reg==15)
                        begin
                            next_state = 3;
                            b_next = 0;
                            count_next = 0;
                        end
                    else
                        b_next = b_reg + 1;
            end

            3: begin
                tx_next = data_reg[0];

                if(b_tick)
                    if(b_reg == 15) begin
                        b_next = 0;
                        data_next = data_reg >> 1;
                        if(count_reg == 8)    
                            next_state = 4;
                        else
                            count_next = count_reg + 1;
                    end else
                        b_next = b_reg + 1;
            end

            4: begin
                tx_next = 1'b1;
                if(b_tick)
                    if(b_reg == 15) begin
                        next_state = 1;
                        tx_done = 1'b1;
                    end else
                        b_next = b_reg + 1;
            end
        endcase
    end
    
    assign tx = tx_reg;
    
endmodule
"

let mod1 = @"
module d_ff(input clk, input d, output reg q, output reg q_bar);
    always @(posedge clk) begin
        q <= d;
        q_bar <= !d;
    end
endmodule
"

let mod2 = @"
module d_ff(clk, d, q, q_bar);
    input clk, d;
    output reg q, q_bar;
    
    always @(posedge clk) begin
        q <= d;
        q_bar <= !d;
    end
endmodule
"

let fifo = @"
module FIFO
(
  input wire clk,
  input wire resetn,
  input wire rd,
  input wire wr,
  input wire w_data,
  
  output wire empty,
  output wire full,
  output wire r_data
);

//Internal Signal declarations

  reg [7:0] array_reg;
  reg [2:0] w_ptr_reg;
  reg [2:0] w_ptr_next;
  reg [2:0] w_ptr_succ;
  reg [2:0] r_ptr_reg;
  reg [2:0] r_ptr_next;
  reg [2:0] r_ptr_succ;

  reg full_reg;
  reg empty_reg;
  reg full_next;
  reg empty_next;

  wire w_en;

  always @ (posedge clk)
    if(w_en)
    begin
      array_reg[w_ptr_reg] <= w_data;
    end

  assign r_data = array_reg[r_ptr_reg];   

  assign w_en = wr & ~full_reg;           

  //State Machine
  always @ (posedge clk, negedge resetn)
  begin
    if(!resetn)
      begin
        w_ptr_reg <= 0;
        r_ptr_reg <= 0;
        full_reg <= 1'b0;
        empty_reg <= 1'b1;
      end
    else
      begin
        w_ptr_reg <= w_ptr_next;
        r_ptr_reg <= r_ptr_next;
        full_reg <= full_next;
        empty_reg <= empty_next;
      end
  end


  //Next State Logic
  always @*
  begin
    w_ptr_succ = w_ptr_reg + 1;
    r_ptr_succ = r_ptr_reg + 1;
    
    w_ptr_next = w_ptr_reg;
    r_ptr_next = r_ptr_reg;
    full_next = full_reg;
    empty_next = empty_reg;
    
    case({w_en,rd})
      //2'b00: nop
      2'b01:
        if(~empty_reg)
          begin
            r_ptr_next = r_ptr_succ;
            full_next = 1'b0;
            if (r_ptr_succ == w_ptr_reg)
              empty_next = 1'b1;
          end
      2'b10:
        if(~full_reg)
          begin
            w_ptr_next = w_ptr_succ;
            empty_next = 1'b0;
            if (w_ptr_succ == r_ptr_reg)
              full_next = 1'b1;
          end
      2'b11:
        begin
          w_ptr_next = w_ptr_succ;
          r_ptr_next = r_ptr_succ;
        end
    endcase
  end

  //Set Full and Empty

  assign full = full_reg;
  assign empty = empty_reg;
  
endmodule
"

let alu = @"
/* ALU Arithmetic and Logic Operations
----------------------------------------------------------------------
|ALU_Sel|   ALU Operation
----------------------------------------------------------------------
| 0000  |   ALU_Out = A + B;
----------------------------------------------------------------------
| 0001  |   ALU_Out = A - B;
----------------------------------------------------------------------
| 0010  |   ALU_Out = A * B;
----------------------------------------------------------------------
| 0011  |   ALU_Out = A / B;
----------------------------------------------------------------------
| 0100  |   ALU_Out = A << 1;
----------------------------------------------------------------------
| 0101  |   ALU_Out = A >> 1;
----------------------------------------------------------------------
| 0110  |   ALU_Out = A rotated left by 1;
----------------------------------------------------------------------
| 0111  |   ALU_Out = A rotated right by 1;
----------------------------------------------------------------------
| 1000  |   ALU_Out = A and B;
----------------------------------------------------------------------
| 1001  |   ALU_Out = A or B;
----------------------------------------------------------------------
| 1010  |   ALU_Out = A xor B;
----------------------------------------------------------------------
| 1011  |   ALU_Out = A nor B;
----------------------------------------------------------------------
| 1100  |   ALU_Out = A nand B;
----------------------------------------------------------------------
| 1101  |   ALU_Out = A xnor B;
----------------------------------------------------------------------
| 1110  |   ALU_Out = 1 if A>B else 0;
----------------------------------------------------------------------
| 1111  |   ALU_Out = 1 if A=B else 0;
----------------------------------------------------------------------*/
// fpga4student.com: FPGA projects, Verilog projects, VHDL projects
// Verilog project: Verilog code for ALU
// by FPGA4STUDENT
module alu
(
    input [7:0] A,B,  // ALU 8-bit Inputs                 
    input [3:0] ALU_Sel,// ALU Selection
    output [7:0] ALU_Out, // ALU 8-bit Output
    output CarryOut // Carry Out Flag
);
    reg [7:0] ALU_Result;
    wire [8:0] tmp;

    assign ALU_Out = ALU_Result; // ALU out
    assign tmp = {1'b0,A} + {1'b0,B};
    assign CarryOut = tmp[8]; // Carryout flag
    
    always @(*) begin
        case(ALU_Sel)
            4'b0000: // Addition
                ALU_Result = A + B ; 
            4'b0001: // Subtraction
                ALU_Result = A - B ;
            4'b0010: // Multiplication
                ALU_Result = A * B;
            4'b0011: // Division
                ALU_Result = A/B;
            4'b0100: // Logical shift left
                ALU_Result = A<<1;
            4'b0101: // Logical shift right
                ALU_Result = A>>1;
            4'b0110: // Rotate left
                ALU_Result = {A[6:0],A[7]};
            4'b0111: // Rotate right
                ALU_Result = {A[0],A[7:1]};
            4'b1000: //  Logical and 
                ALU_Result = A & B;
            4'b1001: //  Logical or
                ALU_Result = A | B;
            4'b1010: //  Logical xor 
                ALU_Result = A ^ B;
            4'b1011: //  Logical nor
                ALU_Result = ~(A | B);
            4'b1100: // Logical nand 
                ALU_Result = ~(A & B);
            4'b1101: // Logical xnor
                ALU_Result = ~(A ^ B);
            4'b1110: // Greater comparison
                ALU_Result = (A>B)?8'd1:8'd0 ;
            4'b1111: // Equal comparison   
              ALU_Result = (A==B)?8'd1:8'd0 ;
            // default: ALU_Result = A + B ; 
        endcase
    end
endmodule
"

// TODO: fix default thing in case
// TODO: check alu outputs are correct

[<EntryPoint>]
let main _ =
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let startTiming a =
        stopwatch.Restart()
        a
    let stopTiming label a =
        stopwatch.Stop()
        printfn "[%s] elapsed (ms): %i" label stopwatch.ElapsedMilliseconds
        a
        
    let modules = [alu]
    let topLevel = "alu"
    let inputs =
        [ "A", Once [ VNum 5 ]
          "B", Once [ VNum 10 ]
          "ALU_Sel", Once [ VNum 0; VNum 1; VNum 2; VNum 3; VNum 4; VNum 5; VNum 6; VNum 7; VNum 8; VNum 9; VNum 10; VNum 11; VNum 12; VNum 13; VNum 14; VNum 15 ] ]
        |> Map.ofList
    let numberOfCycles = 16u
    let reqVars = [ "A"; "B"; "ALU_Sel"; "ALU_Out"; "CarryOut" ]

    modules
    |> List.map (fun modStr ->
        modStr
        |> startTiming
        |> Parse.sourceText
        |> stopTiming "PAR"
        |> function
        | Success (ast, _, _) -> ast
        | Failure (msg, _, _) -> failwith msg)
    |> startTiming
    |> Compiler.Compile.project topLevel
    |> stopTiming "COM"
    |> function
    | Compiler.CompResult.Fail _ as r -> failwithf "%A" r
    | Compiler.CompResult.Succ s -> 
        // printfn "SUCC:\n %s" <| s.ToString()
        s
    | Compiler.CompResult.Warn (s, w) ->
        // printfn "WARN:\n %s" <| s.ToString()
        printfn "WARNINGS:\n %A" w
        s
    |> startTiming
    |> function
    | netlist -> Simulate.runSimulation netlist inputs reqVars numberOfCycles
    |> stopTiming "SIM"
    |> printfn "%A"
    0 // return an integer exit code
