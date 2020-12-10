with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Containers.Ordered_Sets;
package body AOC2020.Day10 is
   package Integer_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);
   use Integer_Ordered_Sets;
   procedure Read_File(S : out Set) is
      Fin : File_Type;
      N : Integer;
   begin
      S := To_Set (0);
      Open (Fin, In_File, "../input/10.txt");
      while not End_Of_File (Fin) loop
         Get (Fin, N);
         S.Insert (N);
      end loop;
      Close (Fin);
      S.Insert (3 + S.Last_Element);
   end Read_File;
   function Count_Gaps (Data : in Set) return Integer is
      One, Three : Integer := 0;
   begin
      for I in Data.Iterate loop
         if Next(I) /= No_Element
         then
            case Element(Next(I)) - Element(I) is
               when 3 => Three := Three + 1;
               when 2 => null;
               when 1 => One := One + 1;
               when others =>
                  null; -- should never happen
            end case;
         end if;
      end loop;
      return One * Three;
   end Count_Gaps;
   function Count_Paths (Data : in Set) return Long_Long_Integer is
      A, B : Long_Long_Integer := 0;
      C : Long_Long_Integer := 1;
      T : Long_Long_Integer;
   begin
      for I in Data.Iterate loop
         if Next(I) /= No_Element
         then
            case Element(Next(I)) - Element(I) is
               when 3 =>
                  A := 0;
                  B := 0;
                  C := C;
               when 2 =>
                  T := B;
                  A := 0;
                  B := C;
                  C := T + C;
               when 1 =>
                  T := C;
                  C := A + B + C;
                  A := B;
                  B := T;
               when others =>
                  null; -- should never happen
            end case;
         end if;
      end loop;
      return C;
   end Count_Paths;
   procedure Run is
     Data : Set;
   begin
      Read_File (Data);
      Put_Line ("Advent of Code 2020 - Day 10"); New_Line;
      Put_Line ("The result for Part 1 is: " & Count_Gaps (Data)'Image);
      Put_Line ("The result for Part 2 is: " & Count_Paths (Data)'Image);
   end Run;
end AOC2020.Day10;
