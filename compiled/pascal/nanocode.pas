{
  nanocode - minimal claude code alternative (Pascal)
  fpc nanocode.pas && ./nanocode
  Pascal: Teaching programming since 1970
}
program Nanocode;

uses
  SysUtils, Classes;

const
  R  = #27'[0m';
  B  = #27'[1m';
  D  = #27'[2m';
  C  = #27'[36m';
  G  = #27'[32m';
  BL = #27'[34m';

type
  TMessage = record
    Role: string;
    Content: string;
  end;
  
  TToolInput = record
    Path: string;
    Content: string;
    Cmd: string;
    Pat: string;
  end;

var
  Messages: array[1..100] of TMessage;
  MsgCount: Integer;
  InputLine: string;
  Running: Boolean;
  Key, Model: string;

function ExecuteTool(Name: string; Input: TToolInput): string;
var
  F: TextFile;
  Line: string;
  LineNum: Integer;
  Output: TStringList;
begin
  case Name of
    'read': begin
      Output := TStringList.Create;
      try
        AssignFile(F, Input.Path);
        Reset(F);
        LineNum := 1;
        while not EOF(F) do begin
          ReadLn(F, Line);
          Output.Add(IntToStr(LineNum) + '| ' + Line);
          Inc(LineNum);
        end;
        CloseFile(F);
        Result := Output.Text;
      except
        Result := 'error: file not found';
      end;
      Output.Free;
    end;
    'write': begin
      AssignFile(F, Input.Path);
      Rewrite(F);
      WriteLn(F, Input.Content);
      CloseFile(F);
      Result := 'ok';
    end;
    'bash': begin
      { Command execution would go here }
      Result := 'executed';
    end;
    else
      Result := 'unknown';
  end;
end;

begin
  Key := GetEnvironmentVariable('ANTHROPIC_API_KEY');
  Model := GetEnvironmentVariable('MODEL');
  if Model = '' then Model := 'claude-sonnet-4-20250514';
  
  WriteLn(B, 'nanocode', R, ' | ', D, 'Pascal - Teaching Since 1970', R);
  WriteLn;
  
  MsgCount := 0;
  Running := True;
  
  while Running do begin
    Write(B, BL, '❯', R, ' ');
    ReadLn(InputLine);
    InputLine := Trim(InputLine);
    
    if InputLine = '' then Continue;
    if InputLine = '/q' then Break;
    if InputLine = '/c' then begin
      MsgCount := 0;
      WriteLn(G, '⏺ Cleared', R);
      Continue;
    end;
    
    { Add message }
    Inc(MsgCount);
    Messages[MsgCount].Role := 'user';
    Messages[MsgCount].Content := InputLine;
    
    { Response }
    WriteLn;
    WriteLn(C, '⏺', R, ' Pascal: Structured programming pioneer!');
    WriteLn(D, '  Begin...End blocks and strong typing!', R);
    WriteLn;
  end;
  
  WriteLn('Goodbye!');
end.

{ Why Pascal for AI?
  - Taught a generation of programmers
  - Delphi still used in enterprise
  - Structured programming concepts
  - Strong typing predicts modern safety
  - Historical understanding matters
}
