with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_Io;
with Ada.Containers.Vectors;
package body AOC2020.Day05 is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Element_Type => Integer, Index_Type => Natural);
   package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;
   use Integer_Vectors; use Integer_Vectors_Sorting;
   -- Used as an example of matching regular expressions

   function BSP_To_Number (BSP : String) return Integer is
      Result : Integer := 0;
   begin
      for C of BSP loop
         case C is
            when 'F' | 'L' =>
              Result := Result * 2;
            when 'B' | 'R' =>
               Result := Result * 2 + 1;
            when others =>
               null;
         end case;
      end loop;
      return Result;
   end BSP_To_Number;

   procedure Gather_Input(Passes : out Vector) is
      Fin : File_Type;
      Line : String(1..11);
      Length : Natural;
   begin
      Open (Fin, In_File, "../input/05.txt");
      loop
         exit when End_Of_File(Fin);
         Get_Line(Fin, Line, Length);
         Passes.Append(BSP_To_Number(Line));
      end loop;
      Close(Fin);
      null;
   end Gather_Input;

   procedure Run is
      Passes : Vector;
      Max : Integer := Integer'First;
      My_Seat : Integer;
   begin
      Gather_Input(Passes);
      for C in Passes.Iterate loop
         if Passes(C) > Max
         then
            Max := Passes(C);
         end if;
      end loop;
      Sort(Passes);
      for C in Passes.Iterate loop
         exit when Next(C) = No_Element;
         if Passes(Next(C)) - Passes(C) = 2
         then
            My_Seat := Passes(C) + 1;
         end if;
      end loop;
      Put_Line("Advent of Code 2020 - Day 05:"); New_Line;
      Put_Line("The result for part 1 is: " & Max'Image);
      Put_Line("The result for Part 2 is: " & My_Seat'Image);
   end Run;
end AOC2020.Day05;
