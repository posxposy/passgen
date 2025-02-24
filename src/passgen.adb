with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;

procedure PassGen is
   type Argument_Kind is (No_Lower, No_Upper, No_Number, No_Symbol, Help, Unknown);

   Lowercase : constant String := "abcdefghijklmnopqrstuvwxyz";
   Uppercase : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   Numbers   : constant String := "0123456789";
   Symbols   : constant String := "!@#$%^&*()_+-=[]{}|;:,.<>?";

   subtype Password_Length is Integer range 4 .. 128;
   Length_Arg : Password_Length := 12;

   Use_Lower   : Boolean := True;
   Use_Upper   : Boolean := True;
   Use_Digits  : Boolean := True;
   Use_Symbols : Boolean := True;

   type Rand_Range is range 1 .. 100;
   package Random is new Ada.Numerics.Discrete_Random (Rand_Range);
   Gen : Random.Generator;

   function Parse_Arg (Arg : String) return Argument_Kind is
   begin
      if Arg = "--no-lower" then
         return No_Lower;
      elsif Arg = "--no-upper" then
         return No_Upper;
      elsif Arg = "--no-numbers" then
         return No_Number;
      elsif Arg = "--no-symbols" then
         return No_Symbol;
      elsif Arg = "--help" then
         return Help;
      else
         return Unknown;
      end if;
   end Parse_Arg;

   function Random_Char (Source : String) return Character is
   begin
      return Source (Integer (Random.Random (Gen)) mod Source'Length + 1);
   end Random_Char;

   procedure Shuffle (Text : in out Unbounded_String) is
      Temp       : Character;
      Idx1, Idx2 : Integer;
   begin
      for I in reverse 2 .. Length (Text) loop
         Idx1 := I;
         Idx2 := Integer (Random.Random (Gen)) mod I + 1;
         Temp := Element (Text, Idx1);
         Replace_Element (Text, Idx1, Element (Text, Idx2));
         Replace_Element (Text, Idx2, Temp);
      end loop;
   end Shuffle;

   function Generate_Password (Len : Password_Length) return String is
      Result        : Unbounded_String := To_Unbounded_String ("");
      Allowed_Chars : Unbounded_String := To_Unbounded_String ("");
      Remaining_Len : Integer          := Len;
   begin
      if Use_Lower then
         Append (Allowed_Chars, Lowercase);
      end if;
      if Use_Upper then
         Append (Allowed_Chars, Uppercase);
      end if;
      if Use_Digits then
         Append (Allowed_Chars, Numbers);
      end if;
      if Use_Symbols then
         Append (Allowed_Chars, Symbols);
      end if;

      if Use_Lower and Remaining_Len > 0 then
         Append (Result, Random_Char (Lowercase));
         Remaining_Len := Remaining_Len - 1;
      end if;
      if Use_Upper and Remaining_Len > 0 then
         Append (Result, Random_Char (Uppercase));
         Remaining_Len := Remaining_Len - 1;
      end if;
      if Use_Digits and Remaining_Len > 0 then
         Append (Result, Random_Char (Numbers));
         Remaining_Len := Remaining_Len - 1;
      end if;
      if Use_Symbols and Remaining_Len > 0 then
         Append (Result, Random_Char (Symbols));
         Remaining_Len := Remaining_Len - 1;
      end if;

      if Length (Allowed_Chars) = 0 then
         raise Constraint_Error;
      end if;

      for I in 1 .. Remaining_Len loop
         Append (Result, Element (Allowed_Chars, Integer (Random.Random (Gen)) mod Length (Allowed_Chars) + 1));
      end loop;

      Shuffle (Result);
      return To_String (Result);
   end Generate_Password;
begin
   Random.Reset (Gen);
   for I in 1 .. Argument_Count loop
      declare
         Arg  : String        := Argument (I);
         Kind : Argument_Kind := Parse_Arg (Arg);
      begin
         if Arg'Length > 2 and then Arg (1 .. 2) = "--" then
            case Kind is
               when No_Lower =>
                  Use_Lower := False;
               when No_Upper =>
                  Use_Upper := False;
               when No_Number =>
                  Use_Digits := False;
               when No_Symbol =>
                  Use_Symbols := False;
               when Help =>
                  Put_Line ("Usage: PassGen [options] [length]");
                  Put_Line ("Options:");
                  Put_Line ("  --no-lower    Exclude lowercase letters");
                  Put_Line ("  --no-upper    Exclude uppercase letters");
                  Put_Line ("  --no-numbers  Exclude numbers");
                  Put_Line ("  --no-symbols  Exclude symbols");
                  Put_Line ("  --help        Display this help message");
                  return;
               when Unknown =>
                  Put_Line ("Unknown argument " & Arg);
                  Set_Exit_Status (1);
                  return;
            end case;
         else
            begin
               Length_Arg := Password_Length'Value (Arg);
            exception
               when Constraint_Error =>
                  Put_Line ("Password length must be between 4 and 128");
                  Set_Exit_Status (1);
                  return;
               when others           =>
                  Put_Line ("Invalid argument " & Arg & " (expected integer)");
                  Set_Exit_Status (1);
                  return;
            end;
         end if;
      end;
   end loop;

   begin
      declare
         Password : String := Generate_Password (Length_Arg);
      begin
         Put_Line (Password);
      end;
   exception
      when Constraint_Error =>
         Put_Line ("At least one character set must be enabled.");
         Set_Exit_Status (1);
   end;

end PassGen;
