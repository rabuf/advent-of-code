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
   subtype Expiration_Year is Integer range 2020..2030;
   subtype Digit is Character range '0' .. '9';
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

   function Has_Valid_Birth_Year(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("byr");
   begin
      return Passport.Contains(Key)
        and then Integer'Value(To_String(Passport.Element(Key))) in Birth_Year;
   end Has_Valid_Birth_Year;

   function Has_Valid_Issue_Year(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("iyr");
   begin
      return Passport.Contains(Key)
        and then Integer'Value(To_String(Passport.Element(Key))) in Issue_Year;
   end Has_Valid_Issue_Year;

   function Has_Valid_Expiration_Year(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("eyr");
   begin
      return Passport.Contains(Key)
        and then Integer'Value(To_String(Passport.Element(Key))) in Expiration_Year;
   end Has_Valid_Expiration_Year;

   function Has_Valid_Hair_Color(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("hcl");
   begin
      if Passport.Contains(Key)
      then
         declare
            S : String := To_String(Passport.Element(Key));
         begin
            if S'Length /= 7 then return False; end if;
            if S(1) /= '#' then return False; end if;
            for C of S(2..7) loop
               if C not in Digit and C not in Hex_Letter
               then return False;
               end if;
            end loop;
         end;
      else
         return False;
      end if;
      return True;
   end Has_Valid_Hair_Color;

   function Has_Valid_Eye_Color(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("ecl");
      Valid_Hair : array (1..7) of String(1..3) := ("amb", "blu", "brn", "gry", "grn", "hzl", "oth");
      Valid : Boolean := False;
   begin
      if Passport.Contains(Key)
      then
         declare
            S : String := To_String(Passport.Element(Key));
         begin
            if S'Length /= 3 then return False; end if;
            for Color of Valid_hair loop
               Valid := Valid or Color = S;
            end loop;
         end;
      else
         return False;
      end if;
      return Valid;
   end Has_Valid_Eye_Color;

   function Has_Valid_PID(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("pid");
   begin
      if Passport.Contains(Key)
      then
         declare
            S : String := To_String(Passport.Element(Key));
         begin
            if S'Length /= 9 then return False; end if;
            for C of S loop
               if C not in Digit
               then return False;
               end if;
            end loop;
         end;
      else
         return False;
      end if;
      return True;
   end Has_Valid_PID;

   function Has_Valid_Height(Passport : Map) return Boolean is
      Key : Unbounded_String := To_Unbounded_String("hgt");
   begin
      if Passport.Contains(Key)
      then
         declare
            S : String := To_String(Passport.Element(Key));
            H : Natural;
         begin
            if S'Length < 4 then return False; end if;
            for C in 1..S'Last-2 loop
               if S(C) not in Digit
               then return False;
               end if;
            end loop;
            H := Integer'Value(S(1..S'Last-2));
            if S(S'Last-1..S'Last) = "cm"
            then
               return H in 150..193;
            elsif S(S'Last-1..S'Last) = "in"
            then
               return H in 59..76;
            end if;
         end;
      else
         return False;
      end if;
      return False;
   end Has_Valid_Height;

   function Has_Valid_Fields(Passport : Map) return Boolean is
   begin
      return Has_Valid_Birth_Year(Passport) and Has_Valid_Issue_Year(Passport)
        and Has_Valid_Expiration_Year(Passport) and Has_Valid_Hair_Color(Passport)
        and Has_Valid_Eye_Color(Passport) and Has_Valid_PID(Passport)
        and Has_Valid_Height(Passport);
   end Has_Valid_Fields;

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
      Valid := 0;
      for C in Passports.Iterate loop
         if  Has_Valid_Fields(Passports(C))
         then
            Valid := Valid + 1;
         end if;
      end loop;
      Put_Line("The result for Part 2 is: " & Valid'Image);
   end Run;
end AOC2020.Day04;
