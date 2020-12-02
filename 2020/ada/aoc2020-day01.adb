with Ada.Containers.Hashed_Sets;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body AOC2020.Day01 is

   function Hash (Value : Integer) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod(Value));
   package Integer_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Integer,
      Hash => Hash,
      Equivalent_Elements => "=");
   use Integer_Sets;

   Input : Set;

   function Solve_01 return Integer is
      Result : Integer := -1;
   begin
      for I in Input.Iterate loop
         if Input.Contains(2020 - Input(I))
         then
            return Input(I) * (2020 - Input(I));
         end if;
      end loop;
      return Result;
   end Solve_01;

   function Solve_02 return Integer is
      Result : Integer := -1;
   begin
      for I in Input.Iterate loop
         for J in Input.Iterate loop
            if Input.Contains (2020 - Input(I) - Input(J))
            then
               return (2020 - Input(I) - Input(J)) * Input(I) * Input(J);
            end if;
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
         Input.Insert (Line);
      end loop;
   end;
   Put_Line("Advent of Code 2020 - Day 01:"); New_Line;
   Put_Line("The result for part 1 is: " & Integer'Image(Solve_01));
   Put_Line("The result for Part 2 is: " & Integer'Image(Solve_02));
end AOC2020.Day01;
