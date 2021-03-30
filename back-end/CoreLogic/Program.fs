open System
open Parser
open Parser.ConstantExpression
open Parser.Expression
open Parser.LangConstructs
open Parser.Token
open Parser.Utils
open FParsec

let program = @"
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
  
  always @(posedge clk, negedge resetn)
  begin
    if(!resetn)
      begin
        current_state <= idle_st;
        b_reg <= 0;
        count_reg <= 0;
        data_reg <= 0;
        tx_reg <= 1'b1;
      end
    else
      begin
        current_state <= next_state;
        b_reg <= b_next;
        count_reg <= count_next;
        data_reg <= data_next;
        tx_reg <= tx_next;
      end
  end


  always @*
  begin
    next_state = current_state;
    tx_done = 1'b0;
    b_next = b_reg;
    count_next = count_reg;
    data_next = data_reg;
    tx_next = tx_reg;
    
    case(current_state)
      idle_st:
      begin
        tx_next = 1'b1;
        if(tx_start)
        begin
          next_state = start_st;
          b_next = 0;
          data_next = d_in;
        end
      end
      
      start_st: 
      begin
        tx_next = 1'b0;
        if(b_tick)
          if(b_reg==15)
            begin
              next_state = data_st;
              b_next = 0;
              count_next = 0;
            end
          else
            b_next = b_reg + 1;
      end
      
      data_st: 
      begin
        tx_next = data_reg[0];
        
        if(b_tick)
          if(b_reg == 15)
            begin
              b_next = 0;
              data_next = data_reg >> 1;
              if(count_reg == 8)    
                next_state = stop_st;
              else
                count_next = count_reg + 1;
            end
          else
            b_next = b_reg + 1;
      end
      
      stop_st: 
      begin
        tx_next = 1'b1;
        if(b_tick)
          if(b_reg == 15)  
            begin
              next_state = idle_st;
              tx_done = 1'b1;
            end
          else
            b_next = b_reg + 1;
      end
    endcase
  end
  
  assign tx = tx_reg;
  
endmodule
"

let testStr = @"
    
    case({w_en,rd})
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
"

[<EntryPoint>]
let main argv =
    let parser = pSourceText false
    let result = run parser program
    // let parser = spaces >>. Keyword.pCase >>. Symbol.pOpenRBrac >>. pExpression .>> Symbol.pCloseRBrac .>>. many1 (pCaseItem) .>> Keyword.pEndCase
    // let result = run parser testStr
    printfn "%A" result
    0 // return an integer exit code
