with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;

procedure PassGen is
   type Argument_Kind is (No_Lower, No_Upper, No_Number, No_Symbol, Unknown);

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
      else
         return Unknown;
      end if;
   end Parse_Arg;

   function Generate_Password (Len : Password_Length) return String is
      Result        : Unbounded_String := To_Unbounded_String ("");
      Allowed_Chars : Unbounded_String := To_Unbounded_String ("");
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

      if Length (Allowed_Chars) = 0 then
         Put_Line ("Provide at least one character set.");
         return To_String (Result);
      end if;

      for I in 1 .. Len loop
         declare
            Idx : Integer := Integer (Random.Random (Gen)) mod Length (Allowed_Chars) + 1;
         begin
            Append (Result, Element (Allowed_Chars, Idx));
         end;
      end loop;

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
               when Unknown =>
                  Put_Line ("Unknown argument " & Arg);
                  return;
            end case;
         else
            begin
               Length_Arg := Password_Length'Value (Arg);
            exception
               when Constraint_Error =>
                  Put_Line ("Password length must be between 4 and 128");
                  return;
               when others           =>
                  Put_Line ("Invalid argument " & Arg & " (expected integer)");
                  return;
            end;
         end if;
      end;
   end loop;

   declare
      Password : String := Generate_Password (Length_Arg);
   begin
      Put_Line (Password);
   end;
end PassGen;
