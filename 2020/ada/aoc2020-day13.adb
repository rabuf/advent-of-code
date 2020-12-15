with Text_IO; use Text_IO;
with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
package body AOC2020.Day13 is
   package Bus_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Natural,
      Element_Type => Natural,
      "<" => ">");
   use Bus_Maps;
   procedure Read_File (Time_Stamp : out Integer; Data : out Map) is
      Fin : File_Type;
      Index, Bus : Natural := 0;
      Line : Unbounded_String;
      I : Positive := 1;
      J : Natural := 1;
   begin
      Open (Fin, In_File, "../input/13.txt");
      Get (Fin, Time_Stamp);
      Skip_Line (Fin);
      Get_Line (Fin, Line);
      Close (Fin);
      loop
         -- I don't know why, but I had to fully qualify `Index` here.
         J := Ada.Strings.Unbounded.Index (Source => Line, Pattern => ",", From => I);
         if Element (Line, I) in  '0' .. '9' or J = 0
         then
            Bus := Integer'Value (if J = 0 then Slice (Line, I, Length (Line)) else Slice (Line, I, J-1));
            Data.Insert (Bus, Index);
            Index := Index + 1;
         end if;
         I := J + 1;
         exit when J = 0;
      end loop;
   end Read_File;
   function Next_Bus (Timestamp : in Integer; Buses : in Map) return Integer is
      Wait : Integer := Integer'Last;
      Bus : Integer := 0;
      TW, TB : Integer := 0;
   begin
      for C in Buses.Iterate loop
         TB := Key ( C);
         TW := TB - (Timestamp mod TB);
         if TW < Wait
         then
            Wait := TW;
            Bus := TB;
         end if;
      end loop;
      return Wait * Bus;
   end Next_Bus;
   function Find_Time (Timestamp : in Integer; Buses : in Map) return Long_Long_Integer is
      T : Long_Long_Integer := 1;
      N : Long_Long_Integer := 1;
   begin
      for C in Buses.Iterate loop
         loop
            exit when 0 = (T + Long_Long_Integer(Element (C))) mod Long_Long_Integer(Key (C));
            T := T + N;
         end loop;
         N := Long_Long_Integer(Key (C)) * N;
      end loop;
      return T;
   end Find_Time;
   procedure Run is
      Timestamp : Integer;
      Buses : Map;
   begin
      Read_File (Timestamp, Buses);
      Put_Line("Advent of Code 2020 - Day 13");
      Put_Line("The result for Part 1 is " & Integer'Image (Next_Bus (Timestamp, Buses)));
      Put_Line("The result for Part 2 is " & Long_Long_Integer'Image (Find_Time (Timestamp, Buses)));
   end Run;
   
end AOC2020.Day13;
