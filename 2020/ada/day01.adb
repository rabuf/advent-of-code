with Ada.Containers.Vectors;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Day01 is

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Integer);
   use Integer_Vectors;

   Input : Vector;

   function Solve_01 return Integer is
      Result : Integer := -1;
   begin
      for I in Input.First_Index .. (Input.Last_Index - 1) loop
         for J in (I + 1) .. Input.Last_Index loop
            if Input (I) + Input (J) = 2020
            then
               return Input (I) * Input (J);
            end if;
         end loop;
      end loop;
      return Result;
   end Solve_01;

   function Solve_02 return Integer is
      Result : Integer := -1;
   begin
      for I in Input.First_Index .. (Input.Last_Index - 2) loop
         for J in (I + 1) .. (Input.Last_Index - 1) loop
            for K in (J + 1) .. Input.Last_Index loop
               if Input (I) + Input (J) + Input (K) = 2020
               then
                  return Input (I) * Input (J) * Input (K);
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Solve_02;

begin
   declare
      input_file : file_type;
      line : integer;
   begin
      Open (Input_File, in_file, "../input/01.txt");
      loop
         exit when end_of_file (input_file);
         Get (input_file, Line);
         Input.Append (Line);
      end loop;
   end;
   Put_Line("The result for part 1 is: " & Integer'Image(Solve_01));
   Put_Line("The result for Part 2 is: " & Integer'Image(Solve_02));
end Day01;
