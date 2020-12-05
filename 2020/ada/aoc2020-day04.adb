with GNAT.Regpat; use GNAT.Regpat;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Text_IO; use Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body AOC2020.Day04 is
   subtype Birth_Year is Integer range 1920..2002;
   subtype Issue_Year is Integer  range 2010..2020;
   subtype Expire_Year is Integer range 2020..2030;
   subtype Digit is Character range '0' .. '9';
   subtype Lower_Case is Character range 'a' .. 'z';
   subtype Hex_Letter is Character range 'a' .. 'f';
   package Passport_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Unbounded_String, Element_Type => Unbounded_String);
   use Passport_Maps;
   package Passport_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Map);
   use Passport_Vectors;

   procedure Parse(Passports : out Vector) is
      F : File_Type;
      Passport : Map;
   begin
      Open(F, In_File,  "../input/04.txt");
      loop
         exit when End_Of_File(F);
         declare
            Line : constant String := Get_Line(F);
            I : Positive := 1;
            Field_Name : Unbounded_String;
            Field_Value : Unbounded_String;
         begin
            if Line'Length = 0
            then
               Passports.Append(Passport.Copy);
               Passport.Clear;
            else
               I := 1;
               for C in Line'Range loop
                  if Line(C) = ':'
                  then
                     Field_Name := To_Unbounded_String(Line(I..C-1));
                     I := C + 1;
                  elsif Line(C) = ' '
                  then
                     Field_Value := To_Unbounded_String(Line(I..C-1));
                     I := C + 1;
                     Passport.Insert(Field_Name, Field_Value);
                  end if;
               end loop;
               Field_Value := To_Unbounded_String(Line(I..Line'Last));
               Passport.Insert(Field_Name, Field_Value);
            end if;
         end;
      end loop;
      Passports.Append(Passport);
      Close(F);
   end Parse;

   function Has_Mandatory_Fields(Passport : Map) return Boolean is
      Valid : Boolean := True;
      Keys : array (1..7) of String(1..3) := ("byr", "eyr", "iyr", "hgt", "ecl", "hcl", "pid");
   begin
      for K of Keys loop
         Valid := Valid and Contains(Passport,To_Unbounded_String(K));
      end loop;
      return Valid;
   end Has_Mandatory_Fields;

   procedure Run is
      Passports : Vector;
      Valid : Natural;
   begin
      Parse (Passports);
      Put_Line("Advent of Code 2020 - Day 04:"); New_Line;
      Valid := 0;
      for C in Passports.Iterate loop
         if  Has_Mandatory_Fields(Passports(C))
         then
            Valid := Valid + 1;
         end if;
      end loop;
      Put_Line("The result for part 1 is: " & Valid'Image);
      --Put_Line("The result for Part 2 is: " & Unsigned_Integer'Image(Solve_02));
   end Run;
end AOC2020.Day04;
