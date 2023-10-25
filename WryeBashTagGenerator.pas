{
  Generates bash tags for a selected plugin automatically

  Games:  FO3/FNV/FO4/TES4/TES5/SSE/Enderal/EnderalSE
  Author: fireundubh <fireundubh@gmail.com>
  Hotkey: F12
}


Unit WryeBashTagGenerator;

Const 
  ScriptName    = 'WryeBashTagGenerator';
  ScriptVersion = '1.6.4.9';
  ScriptAuthor  = 'fireundubh';
  ScriptEmail   = 'fireundubh@gmail.com';
  ScaleFactor   = Screen.PixelsPerInch / 96;

  FilePath    = DataPath + 'BashTags\';


Var 
  slBadTags        : TStringList;
  slDifferentTags  : TStringList;
  slExistingTags   : TStringList;
  slLog            : TStringList;
  slSuggestedTags  : TStringList;
  slDeprecatedTags : TStringList;
  slOutToFileTags  : TStringList;

  g_FileName       : string;
  g_Tag            : string;
  g_AddTags        : boolean;
  g_AddFile        : boolean;
  g_LogTests       : boolean;


Function wbIsOblivion: boolean;
Begin
  Result := wbGameMode = 1;
End;


Function wbIsSkyrim: boolean;
Begin
  Result := (wbGameMode = 4) Or (wbGameMode = 5) Or (wbGameMode = 7) Or (wbGameMode = 8) Or (wbGameMode = 9);
End;


Function wbIsSkyrimSE: boolean;
Begin
  Result := (wbGameMode = 7) Or (wbGameMode = 9);
End;


Function wbIsFallout3: boolean;
Begin
  Result := wbGameMode = 2;
End;


Function wbIsFalloutNV: boolean;
Begin
  Result := wbGameMode = 3;
End;


Function wbIsFallout4: boolean;
Begin
  Result := (wbGameMode = 6) Or (wbGameMode = 10);
End;


Function wbIsFallout76: boolean;
Begin
  Result := wbGameMode = 11;
End;


Function wbIsEnderal: boolean;
Begin
  Result := wbGameMode = 5;
End;


Function wbIsEnderalSE: boolean;
Begin
  Result := wbGameMode = 9;
End;


Procedure LogInfo(AText: String);
Begin
  AddMessage('[INFO] ' + AText);
End;


Procedure LogWarn(AText: String);
Begin
  AddMessage('[WARN] ' + AText);
End;


Procedure LogError(AText: String);
Begin
  AddMessage('[ERRO] ' + AText);
End;


Function Initialize: integer;
Begin
  ClearMessages();

  LogInfo('--------------------------------------------------------------------------------');
  LogInfo(ScriptName + ' v' + ScriptVersion + ' by ' + ScriptAuthor + ' <' + ScriptEmail + '>');
  LogInfo('--------------------------------------------------------------------------------');
  LogInfo(FilePath);


  g_AddTags  := True;
  g_AddFile  := False;
  g_LogTests := True;

  slLog := TStringList.Create;
  slLog.Sorted     := False;
  slLog.Duplicates := dupAccept;

  slSuggestedTags := TStringList.Create;
  slSuggestedTags.Sorted     := True;
  slSuggestedTags.Duplicates := dupIgnore;
  slSuggestedTags.Delimiter  := ',';

  slExistingTags := TStringList.Create;

  slDifferentTags := TStringList.Create;
  slDifferentTags.Sorted     := True;
  slDifferentTags.Duplicates := dupIgnore;

  slBadTags := TStringList.Create;

  slDeprecatedTags := TStringList.Create;
  slDeprecatedTags.CommaText := 'Body-F,Body-M,Body-Size-F,Body-Size-M,C.GridFlags,Derel,Eyes,Eyes-D,Eyes-E,Eyes-R,Hair,Invent,InventOnly,Merge,Npc.EyesOnly,Npc.HairOnly,NpcFaces,R.Relations,Relations,ScriptContents';

  // Wish I didn't have to make a new list for this, but script errors on AssignFile
  slOutToFileTags := TStringList.Create;

  If ShowPrompt(ScriptName + ' v' + ScriptVersion) = mrAbort Then
    Begin
      LogError('Cannot proceed because user aborted execution');
      Result := 1;
      Exit;
    End;

  If wbIsFallout76 Then
    Begin
      LogError('Cannot proceed because CBash does not support Fallout 76');
      Result := 2;
      Exit;
    End;

  If wbIsFallout3 Then
    LogInfo('Using game mode: Fallout 3')
  Else If wbIsFalloutNV Then
         LogInfo('Using game mode: Fallout: New Vegas')
  Else If wbIsFallout4 Then
         LogInfo('Using game mode: Fallout 4')
  Else If wbIsOblivion Then
         LogInfo('Using game mode: Oblivion')
  Else If wbIsEnderal Then
         LogInfo('Using game mode: Enderal')
  Else If wbIsEnderalSE Then
         LogInfo('Using game mode: Enderal Special Edition')
  Else If wbIsSkyrimSE Then
         LogInfo('Using game mode: Skyrim Special Edition')
  Else If wbIsSkyrim Then
         LogInfo('Using game mode: Skyrim')
  Else
    Begin
      LogError('Cannot proceed because script does not support game mode');
      Result := 3;
      Exit;
    End;

  ScriptProcessElements := [etFile];
End;


Function Process(input: IInterface): integer;

Var 
  kDescription : IwbElement;
  kHeader      : IwbElement;
  sDescription : string;
  sTags        : string;
  sMasterName  : string;
  r            : IwbMainRecord;
  i            : integer;
  f            : IwbFile;
  outFile      : TextFile;
Begin

  If (ElementType(input) = etMainRecord) Then
    exit;

  f := GetFile(input);

  g_FileName := GetFileName(f);

  AddMessage(#10);

  LogInfo('Processing... Please wait. This could take a while.');

  For i := 0 To Pred(RecordCount(f)) Do
    ProcessRecord(RecordByIndex(f, i));

  LogInfo('--------------------------------------------------------------------------------');
  LogInfo(g_FileName);
  LogInfo('-------------------------------------------------------------------------- TESTS');

  If g_LogTests Then
    For i := 0 To Pred(slLog.Count) Do
      LogInfo(slLog[i]);

  LogInfo('------------------------------------------------------------------------ RESULTS');

  If slSuggestedTags.Count > 0 Then
    Begin
      kHeader      := ElementBySignature(f, 'TES4');
      kDescription := ElementBySignature(kHeader, 'SNAM');
      sDescription := GetEditValue(kDescription);

      slExistingTags.CommaText := RegExMatchGroup('{{BASH:(.*?)}}', sDescription, 1);

      StringListIntersection(slExistingTags, slDeprecatedTags, slBadTags);
      LogInfo(FormatTags(slBadTags, 'deprecated tag found:', 'deprecated tags found:', 'No deprecated tags found.'));
      slBadTags.Clear;

      StringListDifference(slSuggestedTags, slExistingTags, slDifferentTags);
      StringListDifference(slExistingTags, slSuggestedTags, slBadTags);
      slSuggestedTags.AddStrings(slDifferentTags);

      If (SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) And Not g_AddFile) Then
        Begin
          LogInfo(FormatTags(slExistingTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.'));
          LogInfo(FormatTags(slSuggestedTags, 'suggested tag:', 'suggested tags:', 'No suggested tags.'));
          LogWarn('No tags to add.' + #13#10);
          Exit;
        End;

      If g_AddTags Then
        Begin
          // if the description element doesn't exist, add the element
          kDescription := ElementBySignature(kHeader, 'SNAM');
          If Not Assigned(kDescription) Then
            kDescription := Add(kHeader, 'SNAM', True);

          sDescription := GetEditValue(kDescription);
          sTags        := Format('{{BASH:%s}}', [slSuggestedTags.DelimitedText]);

          If (Length(sDescription) = 0) And (slSuggestedTags.Count > 0) Then
            sDescription := sTags
          Else If Not SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) Then
                 Begin
                   If slExistingTags.Count = 0 Then
                     sDescription := sDescription + #10#10 +sTags
                   Else
                     sDescription := RegExReplace('{{BASH:.*?}}', sTags, sDescription);
                 End;

          SetEditValue(kDescription, sDescription);

          LogInfo(FormatTags(slBadTags,       'bad tag removed:',          'bad tags removed:',          'No bad tags found.'));
          LogInfo(FormatTags(slDifferentTags, 'tag added to file header:', 'tags added to file header:', 'No tags added.'));
        End
      Else
        Begin
          LogInfo(FormatTags(slBadTags,       'bad tag found:',         'bad tags found:',         'No bad tags found.'));
          LogInfo(FormatTags(slDifferentTags, 'suggested tag to add:',  'suggested tags to add:',  'No suggested tags to add.'));
        End;
      LogInfo(FormatTags(slExistingTags,  'existing tag found:',    'existing tags found:',    'No existing tags found.'));
      LogInfo(FormatTags(slSuggestedTags, 'suggested tag overall:', 'suggested tags overall:', 'No suggested tags overall.'));
      If g_AddFile Then // Write tags to text file
        Begin
          slOutToFileTags.Add (slSuggestedTags.DelimitedText);
          slOutToFileTags.SaveToFile (FilePath+ChangeFileExt(g_FileName, '.txt'));
          slOutToFileTags.Clear;
          LogInfo('Finished writing auggested bash tags to file');
        End
    End
  Else
    LogInfo('No tags are suggested for this plugin.');



  slLog.Clear;
  slSuggestedTags.Clear;
  slExistingTags.Clear;
  slDifferentTags.Clear;
  slBadTags.Clear;

  AddMessage(#10);
End;


Function ProcessRecord(e: IwbMainRecord): integer;

Var 
  o             : IwbMainRecord;
  sSignature    : string;
  ConflictState : TConflictThis;
  iFormID       : integer;
Begin
  ConflictState := ConflictAllForMainRecord(e);

  If (ConflictState = caUnknown)
     Or (ConflictState = caOnlyOne)
     Or (ConflictState = caNoConflict) Then
    Exit;

  // exit if the record should not be processed
  If SameText(g_FileName, 'Dawnguard.esm') Then
    Begin
      iFormID := GetLoadOrderFormID(e) And $00FFFFFF;
      If (iFormID = $00016BCF)
         Or (iFormID = $0001EE6D)
         Or (iFormID = $0001FA4C)
         Or (iFormID = $00039F67)
         Or (iFormID = $0006C3B6) Then
        Exit;
    End;

  // get master record if record is an override
  o := Master(e);

  If Not Assigned(o) Then
    Exit;

  // if record overrides several masters, then get the last one
  o := HighestOverrideOrSelf(o, OverrideCount(o));

  If Equals(e, o) Then
    Exit;

  // stop processing deleted records to avoid errors
  If GetIsDeleted(e)
     Or GetIsDeleted(o) Then
    Exit;

  sSignature := Signature(e);

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FNV
  // -------------------------------------------------------------------------------
  If wbIsFalloutNV Then
    If sSignature = 'WEAP' Then
      ProcessTag('WeaponMods', e, o);

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to TES4
  // -------------------------------------------------------------------------------
  If wbIsOblivion Then
    Begin
      If ContainsStr('CREA NPC_', sSignature) Then
        Begin
          ProcessTag('Actors.Spells', e, o);

          If sSignature = 'CREA' Then
            ProcessTag('Creatures.Blood', e, o);
        End

      Else If sSignature = 'RACE' Then
             Begin
               ProcessTag('R.ChangeSpells', e, o);
               ProcessTag('R.Attributes-F', e, o);
               ProcessTag('R.Attributes-M', e, o);
             End

      Else If sSignature = 'ROAD' Then
             ProcessTag('Roads', e, o)

      Else If sSignature = 'SPEL' Then
             ProcessTag('SpellStats', e, o);
    End;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to TES5, SSE
  // -------------------------------------------------------------------------------
  If wbIsSkyrim Then
    Begin
      If sSignature = 'CELL' Then
        Begin
          ProcessTag('C.Location', e, o);
          ProcessTag('C.LockList', e, o);
          ProcessTag('C.Regions', e, o);
          ProcessTag('C.SkyLighting', e, o);
        End

      Else If ContainsStr('ACTI ALCH AMMO ARMO BOOK FLOR FURN INGR KEYM LCTN MGEF MISC NPC_ SCRL SLGM SPEL TACT WEAP', sSignature) Then
             ProcessTag('Keywords', e, o)

      Else If sSignature = 'FACT' Then
             Begin
               ProcessTag('Relations.Add', e, o);
               ProcessTag('Relations.Change', e, o);
               ProcessTag('Relations.Remove', e, o);
             End

      Else If sSignature = 'NPC_' Then
             Begin
               ProcessTag('Actors.Perks.Add', e, o);
               ProcessTag('Actors.Perks.Change', e, o);
               ProcessTag('Actors.Perks.Remove', e, o);
               ProcessTag('Factions', e, o);

               g_Tag := 'NPC.AIPackageOverrides';
               If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) Then
                 ProcessTag('NPC.AIPackageOverrides', e, o);

               ProcessTag('NPC.AttackRace', e, o);
               ProcessTag('NPC.CrimeFaction', e, o);
               ProcessTag('NPC.DefaultOutfit', e, o);
             End

      Else If sSignature = 'OTFT' Then
             Begin
               ProcessTag('Outfits.Add', e, o);
               ProcessTag('Outfits.Remove', e, o);
             End;
    End;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV
  // -------------------------------------------------------------------------------
  If wbIsFallout3 Or wbIsFalloutNV Then
    Begin
      If sSignature = 'FLST' Then
        ProcessTag('Deflst', e, o);

      g_Tag := 'Destructible';
      If ContainsStr('ACTI ALCH AMMO BOOK CONT DOOR FURN IMOD KEYM MISC MSTT PROJ TACT TERM WEAP', sSignature) Then
        ProcessTag('Destructible', e, o)

        // special handling for CREA and NPC_ record types
      Else If ContainsStr('CREA NPC_', sSignature) Then
             If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) Then
               ProcessTag('Destructible', e, o)

               // added in Wrye Bash 307 Beta 6
      Else If sSignature = 'FACT' Then
             Begin
               ProcessTag('Relations.Add', e, o);
               ProcessTag('Relations.Change', e, o);
               ProcessTag('Relations.Remove', e, o);
             End;
    End;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES4
  // -------------------------------------------------------------------------------
  If wbIsFallout3 Or wbIsFalloutNV Or wbIsOblivion Then
    Begin
      If ContainsStr('CREA NPC_', sSignature) Then
        Begin
          If sSignature = 'CREA' Then
            ProcessTag('Creatures.Type', e, o);

          g_Tag := 'Factions';
          If wbIsOblivion Or Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Factions', False, False) Then
            ProcessTag('Factions', e, o);

          If sSignature = 'NPC_' Then
            Begin
              ProcessTag('NPC.Eyes', e, o);
              ProcessTag('NPC.FaceGen', e, o);
              ProcessTag('NPC.Hair', e, o);
            End;
        End

      Else If sSignature = 'RACE' Then
             Begin
               ProcessTag('R.Body-F', e, o);
               ProcessTag('R.Body-M', e, o);
               ProcessTag('R.Body-Size-F', e, o);
               ProcessTag('R.Body-Size-M', e, o);
               ProcessTag('R.Eyes', e, o);
               ProcessTag('R.Hair', e, o);
               ProcessTag('R.Relations.Add', e, o);
               ProcessTag('R.Relations.Change', e, o);
               ProcessTag('R.Relations.Remove', e, o);
             End;
    End;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES5, SSE
  // -------------------------------------------------------------------------------
  If wbIsFallout3 Or wbIsFalloutNV Or wbIsSkyrim Then
    Begin
      If ContainsStr('CREA NPC_', sSignature) Then
        Begin
          g_Tag := 'Actors.ACBS';
          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Stats', False, False) Then
            ProcessTag('Actors.ACBS', e, o);

          g_Tag := 'Actors.AIData';
          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Data', False, False) Then
            ProcessTag('Actors.AIData', e, o);

          g_Tag := 'Actors.AIPackages';
          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) Then
            ProcessTag('Actors.AIPackages', e, o);

          If sSignature = 'CREA' Then
            If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) Then
              ProcessTag('Actors.Anims', e, o);

          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) Then
            Begin
              ProcessTag('Actors.CombatStyle', e, o);
              ProcessTag('Actors.DeathItem', e, o);
            End;

          g_Tag := 'Actors.Skeleton';
          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) Then
            ProcessTag('Actors.Skeleton', e, o);

          g_Tag := 'Actors.Stats';
          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Stats', False, False) Then
            ProcessTag('Actors.Stats', e, o);

          If wbIsFallout3 Or wbIsFalloutNV Or (sSignature = 'NPC_') Then
            ProcessTag('Actors.Voice', e, o);

          If sSignature = 'NPC_' Then
            Begin
              g_Tag := 'NPC.Class';
              If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) Then
                ProcessTag('NPC.Class', e, o);

              g_Tag := 'NPC.Race';
              If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) Then
                ProcessTag('NPC.Race', e, o);
            End;

          g_Tag := 'Scripts';
          If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Script', False, False) Then
            ProcessTag(g_Tag, e, o);
        End;

      If sSignature = 'CELL' Then
        Begin
          ProcessTag('C.Acoustic', e, o);
          ProcessTag('C.Encounter', e, o);
          ProcessTag('C.ForceHideLand', e, o);
          ProcessTag('C.ImageSpace', e, o);
        End;

      If sSignature = 'RACE' Then
        Begin
          ProcessTag('R.Ears', e, o);
          ProcessTag('R.Head', e, o);
          ProcessTag('R.Mouth', e, o);
          ProcessTag('R.Teeth', e, o);
          ProcessTag('R.Skills', e, o);
          ProcessTag('R.Description', e, o);
          ProcessTag('Voice-F', e, o);
          ProcessTag('Voice-M', e, o);
        End;

      If ContainsStr('ACTI ALCH ARMO CONT DOOR FLOR FURN INGR KEYM LIGH LVLC MISC QUST WEAP', sSignature) Then
        ProcessTag('Scripts', e, o);
    End;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES4, TES5, SSE
  // -------------------------------------------------------------------------------
  If wbIsFallout3 Or wbIsFalloutNV Or wbIsOblivion Or wbIsSkyrim Then
    Begin
      If sSignature = 'CELL' Then
        Begin
          ProcessTag('C.Climate', e, o);
          ProcessTag('C.Light', e, o);
          ProcessTag('C.MiscFlags', e, o);
          ProcessTag('C.Music', e, o);
          ProcessTag('C.Name', e, o);
          ProcessTag('C.Owner', e, o);
          ProcessTag('C.RecordFlags', e, o);
          ProcessTag('C.Water', e, o);
        End;

      // TAG: Delev, Relev
      If ContainsStr('LVLC LVLI LVLN LVSP', sSignature) Then
        ProcessDelevRelevTags(e, o);

      If ContainsStr('ACTI ALCH AMMO APPA ARMO BOOK BSGN CLAS CLOT DOOR FLOR FURN INGR KEYM LIGH MGEF MISC SGST SLGM WEAP', sSignature) Then
        Begin
          ProcessTag('Graphics', e, o);
          ProcessTag('Names', e, o);
          ProcessTag('Stats', e, o);

          If ContainsStr('ACTI DOOR LIGH MGEF', sSignature) Then
            Begin
              ProcessTag('Sound', e, o);

              If sSignature = 'MGEF' Then
                ProcessTag('EffectStats', e, o);
            End;
        End;

      If ContainsStr('CREA EFSH GRAS LSCR LTEX REGN STAT TREE', sSignature) Then
        ProcessTag('Graphics', e, o);

      If sSignature = 'CONT' Then
        Begin
          ProcessTag('Invent.Add', e, o);
          ProcessTag('Invent.Change', e, o);
          ProcessTag('Invent.Remove', e, o);
          ProcessTag('Names', e, o);
          ProcessTag('Sound', e, o);
        End;

      If ContainsStr('DIAL ENCH EYES FACT HAIR QUST RACE SPEL WRLD', sSignature) Then
        Begin
          ProcessTag('Names', e, o);

          If sSignature = 'ENCH' Then
            ProcessTag('EnchantmentStats', e, o);
        End;

      If sSignature = 'FACT' Then
        Begin
          ProcessTag('Relations.Add', e, o);
          ProcessTag('Relations.Change', e, o);
          ProcessTag('Relations.Remove', e, o);
        End;

      If (sSignature = 'WTHR') Then
        ProcessTag('Sound', e, o);

      // special handling for CREA and NPC_
      If ContainsStr('CREA NPC_', sSignature) Then
        Begin
          If wbIsOblivion Or wbIsFallout3 Or wbIsFalloutNV Or (sSignature = 'NPC_') Then
            ProcessTag('Actors.RecordFlags', e, o);

          If wbIsOblivion Then
            Begin
              ProcessTag('Invent.Add', e, o);
              ProcessTag('Invent.Change', e, o);
              ProcessTag('Invent.Remove', e, o);
              ProcessTag('Names', e, o);

              If sSignature = 'CREA' Then
                ProcessTag('Sound', e, o);
            End;

          If Not wbIsOblivion Then
            Begin
              g_Tag := 'Invent.Add';
              If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) Then
                ProcessTag(g_Tag, e, o);

              g_Tag := 'Invent.Change';
              If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) Then
                ProcessTag(g_Tag, e, o);

              g_Tag := 'Invent.Remove';
              If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) Then
                ProcessTag(g_Tag, e, o);

              // special handling for CREA and NPC_ record types
              g_Tag := 'Names';
              If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Base Data', False, False) Then
                ProcessTag(g_Tag, e, o);

              // special handling for CREA record type
              g_Tag := 'Sound';
              If sSignature = 'CREA' Then
                If Not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) Then
                  ProcessTag(g_Tag, e, o);
            End;
        End;
    End;

  // ObjectBounds
  g_Tag := 'ObjectBounds';

  If wbIsFallout3 And ContainsStr('ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK COBJ CONT CREA DOOR EXPL FURN GRAS IDLM INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP', sSignature) Then
    ProcessTag(g_Tag, e, o);

  If wbIsFalloutNV And ContainsStr('ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK CCRD CHIP CMNY COBJ CONT CREA DOOR EXPL FURN GRAS IDLM IMOD INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP', sSignature) Then
    ProcessTag(g_Tag, e, o);

  If wbIsSkyrim And ContainsStr('ACTI ADDN ALCH AMMO APPA ARMO ARTO ASPC BOOK CONT DOOR DUAL ENCH EXPL FLOR FURN GRAS HAZD IDLM INGR KEYM LIGH LVLI LVLN LVSP MISC MSTT NPC_ PROJ SCRL SLGM SOUN SPEL STAT TACT TREE TXST WEAP', sSignature) Then
    ProcessTag(g_Tag, e, o);

  If wbIsFallout4 And ContainsStr('LVLI LVLN', sSignature) Then
    ProcessTag(g_Tag, e, o);

  // Text
  If Not wbIsFallout4 Then
    Begin
      g_Tag := 'Text';

      If wbIsOblivion And ContainsStr('BOOK BSGN CLAS LSCR MGEF SKIL', sSignature) Then
        ProcessTag(g_Tag, e, o);

      If wbIsFallout3 And ContainsStr('AVIF BOOK CLAS LSCR MESG MGEF NOTE PERK TERM', sSignature) Then
        ProcessTag(g_Tag, e, o);

      If wbIsFalloutNV And ContainsStr('AVIF BOOK CHAL CLAS IMOD LSCR MESG MGEF NOTE PERK TERM', sSignature) Then
        ProcessTag(g_Tag, e, o);

      If wbIsSkyrim And ContainsStr('ALCH AMMO APPA ARMO AVIF BOOK CLAS LSCR MESG MGEF SCRL SHOU SPEL WEAP', sSignature) Then
        ProcessTag(g_Tag, e, o);
    End;
End;


Function Finalize: integer;
Begin
  slLog.Free;
  slSuggestedTags.Free;
  slExistingTags.Free;
  slDifferentTags.Free;
  slBadTags.Free;
  slDeprecatedTags.Free;
  slOutToFileTags.Free;
End;


Function StrToBool(AValue: String): boolean;
Begin
  If (AValue <> '0') And (AValue <> '1') Then
    Result := Nil
  Else
    Result := (AValue = '1');
End;


Function RegExMatchGroup(AExpr: String; ASubj: String; AGroup: integer): string;

Var 
  re     : TPerlRegEx;
Begin
  Result := '';
  re := TPerlRegEx.Create;
  Try
    re.RegEx := AExpr;
    re.Options := [];
    re.Subject := ASubj;
    If re.Match Then
      Result := re.Groups[AGroup];
  Finally
    re.Free;
End;
End;


Function RegExReplace(Const AExpr: String; ARepl: String; ASubj: String): string;

Var 
  re      : TPerlRegEx;
  sResult : string;
Begin
  Result := '';
  re := TPerlRegEx.Create;
  Try
    re.RegEx := AExpr;
    re.Options := [];
    re.Subject := ASubj;
    re.Replacement := ARepl;
    re.ReplaceAll;
    sResult := re.Subject;
  Finally
    re.Free;
    Result := sResult;
End;
End;


Function EditValues(Const AElement: IwbElement): string;

Var 
  kElement : IInterface;
  sName    : string;
  i        : integer;
Begin
  Result := GetEditValue(AElement);

  For i := 0 To Pred(ElementCount(AElement)) Do
    Begin
      kElement := ElementByIndex(AElement, i);
      sName    := Name(kElement);

      If SameText(sName, 'unknown') Or SameText(sName, 'unused') Then
        Continue;

      If Result <> '' Then
        Result := Result + ' ' + EditValues(kElement)
      Else
        Result := EditValues(kElement);
    End;
End;


Function CompareAssignment(AElement: IwbElement; AMaster: IwbElement): boolean;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  If Not Assigned(AElement) And Not Assigned(AMaster) Then
    Exit;

  If Assigned(AElement) And Assigned(AMaster) Then
    Exit;

  AddLogEntry('Assigned', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function CompareElementCount(AElement: IwbElement; AMaster: IwbElement): boolean;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  If ElementCount(AElement) = ElementCount(AMaster) Then
    Exit;

  AddLogEntry('ElementCount', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function CompareElementCountAdd(AElement: IwbElement; AMaster: IwbElement): boolean;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  If ElementCount(AElement) <= ElementCount(AMaster) Then
    Exit;

  AddLogEntry('ElementCountAdd', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function CompareElementCountRemove(AElement: IwbElement; AMaster: IwbElement): boolean;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  If ElementCount(AElement) >= ElementCount(AMaster) Then
    Exit;

  AddLogEntry('ElementCountRemove', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function CompareEditValue(AElement: IwbElement; AMaster: IwbElement): boolean;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  If SameText(GetEditValue(AElement), GetEditValue(AMaster)) Then
    Exit;

  AddLogEntry('GetEditValue', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function CompareFlags(AElement: IwbElement; AMaster: IwbElement; APath: String; AFlagName: String; ASuggest: boolean; ANotOperator: boolean): boolean;

Var 
  x         : IwbElement;
  y         : IwbElement;
  a         : IwbElement;
  b         : IwbElement;
  sa        : string;
  sb        : string;
  sTestName : string;
  bResult   : boolean;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  // flags arrays
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  // individual flags
  a := ElementByName(x, AFlagName);
  b := ElementByName(y, AFlagName);

  // individual flag edit values
  sa := GetEditValue(a);
  sb := GetEditValue(b);

  If ANotOperator Then
    Result := Not SameText(sa, sb)  // only used for Behave Like Exterior, Use Sky Lighting, and Has Water
  Else
    Result := StrToBool(sa) Or StrToBool(sb);

  If ASuggest And Result Then
    Begin
      sTestName := IfThen(ANotOperator, 'CompareFlags:NOT', 'CompareFlags:OR');
      AddLogEntry(sTestName, x, y);
      slSuggestedTags.Add(g_Tag);
    End;
End;


Function CompareKeys(AElement: IwbElement; AMaster: IwbElement): boolean;

Var 
  sElementEditValues : string;
  sMasterEditValues  : string;
  ConflictState      : TConflictThis;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  ConflictState := ConflictAllForMainRecord(ContainingMainRecord(AElement));

  If (ConflictState = caUnknown)
     Or (ConflictState = caOnlyOne)
     Or (ConflictState = caNoConflict) Then
    Exit;

  sElementEditValues := EditValues(AElement);
  sMasterEditValues  := EditValues(AMaster);

  If IsEmptyKey(sElementEditValues) And IsEmptyKey(sMasterEditValues) Then
    Exit;

  If SameText(sElementEditValues, sMasterEditValues) Then
    Exit;

  AddLogEntry('CompareKeys', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function CompareNativeValues(AElement: IwbElement; AMaster: IwbElement; APath: String): boolean;

Var 
  x : IwbElement;
  y : IwbElement;
Begin
  Result := False;

  If TagExists(g_Tag) Then
    Exit;

  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  If GetNativeValue(x) = GetNativeValue(y) Then
    Exit;

  AddLogEntry('CompareNativeValues', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
End;


Function SortedArrayElementByValue(AElement: IwbElement; APath: String; AValue: String): IwbElement;

Var 
  i      : integer;
  kEntry : IwbElement;
Begin
  Result := Nil;
  For i := 0 To Pred(ElementCount(AElement)) Do
    Begin
      kEntry := ElementByIndex(AElement, i);
      If SameText(GetElementEditValues(kEntry, APath), AValue) Then
        Begin
          Result := kEntry;
          Exit;
        End;
    End;
End;


// TODO: natively implemented in 4.1.4
Procedure StringListDifference(ASet: TStringList; AOtherSet: TStringList; AOutput: TStringList);

Var 
  i : integer;
Begin
  For i := 0 To Pred(ASet.Count) Do
    If AOtherSet.IndexOf(ASet[i]) = -1 Then
      AOutput.Add(ASet[i]);
End;


// TODO: natively implemented in 4.1.4
Procedure StringListIntersection(ASet: TStringList; AOtherSet: TStringList; AOutput: TStringList);

Var 
  i : integer;
Begin
  For i := 0 To Pred(ASet.Count) Do
    If AOtherSet.IndexOf(ASet[i]) > -1 Then
      AOutput.Add(ASet[i]);
End;


// TODO: speed this up!
Function IsEmptyKey(AEditValues: String): boolean;

Var 
  i : integer;
Begin
  Result := True;
  For i := 1 To Length(AEditValues) Do
    If AEditValues[i] = '1' Then
      Begin
        Result := False;
        Exit;
      End;
End;


Function FormatTags(ATags: TStringList; ASingular: String; APlural: String; ANull: String): string;
Begin
  If ATags.Count = 1 Then
    Result := IntToStr(ATags.Count) + ' ' + ASingular + #13#10#32#32#32#32#32#32
  Else
    If ATags.Count > 1 Then
      Result := IntToStr(ATags.Count) + ' ' + APlural + #13#10#32#32#32#32#32#32;

  If ATags.Count > 0 Then
    Result := Result + Format(' {{BASH:%s}}', [ATags.DelimitedText])
  Else
    Result := ANull;
End;


Function TagExists(ATag: String): boolean;
Begin
  Result := (slSuggestedTags.IndexOf(ATag) <> -1);
End;


Procedure Evaluate(AElement: IwbElement; AMaster: IwbElement);
Begin
  // exit if the tag already exists
  If TagExists(g_Tag) Then
    Exit;

  // Suggest tag if one element exists while the other does not
  If CompareAssignment(AElement, AMaster) Then
    Exit;

  // exit if the first element does not exist
  If Not Assigned(AElement) Then
    Exit;

  // suggest tag if the two elements are different
  If CompareElementCount(AElement, AMaster) Then
    Exit;

  // suggest tag if the edit values of the two elements are different
  If CompareEditValue(AElement, AMaster) Then
    Exit;

  // compare any number of elements with CompareKeys
  If CompareKeys(AElement, AMaster) Then
    Exit;
End;


Procedure EvaluateAdd(AElement: IwbElement; AMaster: IwbElement);
Begin
  If TagExists(g_Tag) Then
    Exit;

  If Not Assigned(AElement) Then
    Exit;

  // suggest tag if the overriding element has more children than its master
  If CompareElementCountAdd(AElement, AMaster) Then
    Exit;
End;


Procedure EvaluateChange(AElement: IwbElement; AMaster: IwbElement);
Begin
  If TagExists(g_Tag) Then
    Exit;

  If Not Assigned(AElement) Then
    Exit;

  // suggest tag if the two elements and their descendants have different contents
  If CompareKeys(AElement, AMaster) Then
    Exit;
End;


Procedure EvaluateRemove(AElement: IwbElement; AMaster: IwbElement);
Begin
  If TagExists(g_Tag) Then
    Exit;

  If Not Assigned(AElement) Then
    Exit;

  // suggest tag if the master element has more children than its override
  If CompareElementCountRemove(AElement, AMaster) Then
    Exit;
End;


Procedure EvaluateByPath(AElement: IwbElement; AMaster: IwbElement; APath: String);

Var 
  x : IInterface;
  y : IInterface;
Begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  Evaluate(x, y);
End;


Procedure EvaluateByPathAdd(AElement: IwbElement; AMaster: IwbElement; APath: String);

Var 
  x : IInterface;
  y : IInterface;
Begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  EvaluateAdd(x, y);
End;


Procedure EvaluateByPathChange(AElement: IwbElement; AMaster: IwbElement; APath: String);

Var 
  x : IInterface;
  y : IInterface;
Begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  EvaluateChange(x, y);
End;


Procedure EvaluateByPathRemove(AElement: IwbElement; AMaster: IwbElement; APath: String);

Var 
  x : IInterface;
  y : IInterface;
Begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  EvaluateRemove(x, y);
End;


Procedure ProcessTag(ATag: String; e: IInterface; m: IInterface);

Var 
  x          : IInterface;
  y          : IInterface;
  a          : IInterface;
  b          : IInterface;
  j          : IInterface;
  k          : IInterface;
  sSignature : string;
Begin
  g_Tag := ATag;

  If TagExists(g_Tag) Then
    Exit;

  sSignature := Signature(e);

  // Bookmark: Actors.ACBS
  If (g_Tag = 'Actors.ACBS') Then
    Begin
      // assign ACBS elements
      x := ElementBySignature(e, 'ACBS');
      y := ElementBySignature(m, 'ACBS');

      // evaluate Flags if the Use Base Data flag is not set
      a := ElementByName(x, 'Flags');
      b := ElementByName(y, 'Flags');

      If wbIsOblivion And CompareKeys(a, b) Then
        Exit;

      If Not wbIsOblivion And Not CompareFlags(x, y, 'Template Flags', 'Use Base Data', False, False) And CompareKeys(a, b) Then
        Exit;

      // evaluate properties
      EvaluateByPath(x, y, 'Fatigue');
      EvaluateByPath(x, y, 'Level');
      EvaluateByPath(x, y, 'Calc min');
      EvaluateByPath(x, y, 'Calc max');
      EvaluateByPath(x, y, 'Speed Multiplier');
      EvaluateByPath(e, m, 'DATA\Base Health');

      // evaluate Barter Gold if the Use AI Data flag is not set
      If wbIsOblivion Or Not CompareFlags(x, y, 'Template Flags', 'Use AI Data', False, False) Then
        EvaluateByPath(x, y, 'Barter gold');
    End

    // Bookmark: Actors.AIData
  Else If (g_Tag = 'Actors.AIData') Then
         Begin
           // assign AIDT elements
           x := ElementBySignature(e, 'AIDT');
           y := ElementBySignature(m, 'AIDT');

           // evaluate AIDT properties
           EvaluateByPath(x, y, 'Aggression');
           EvaluateByPath(x, y, 'Confidence');
           EvaluateByPath(x, y, 'Energy level');
           EvaluateByPath(x, y, 'Responsibility');
           EvaluateByPath(x, y, 'Teaches');
           EvaluateByPath(x, y, 'Maximum training level');

           // check flags for Buys/Sells and Services
           If CompareNativeValues(x, y, 'Buys/Sells and Services') Then
             Exit;
         End

         // Bookmark: Actors.AIPackages
  Else If (g_Tag = 'Actors.AIPackages') Then
         EvaluateByPath(e, m, 'Packages')

         // Bookmark: Actors.Anims
  Else If (g_Tag = 'Actors.Anims') Then
         EvaluateByPath(e, m, 'KFFZ')

         // Bookmark: Actors.CombatStyle
  Else If (g_Tag = 'Actors.CombatStyle') Then
         EvaluateByPath(e, m, 'ZNAM')

         // Bookmark: Actors.DeathItem
  Else If (g_Tag = 'Actors.DeathItem') Then
         EvaluateByPath(e, m, 'INAM')

         // Bookmark: Actors.Perks.Add (TES5, SSE)
  Else If (g_Tag = 'Actors.Perks.Add') Then
         EvaluateByPathAdd(e, m, 'Perks')

         // Bookmark: Actors.Perks.Change (TES5, SSE)
  Else If (g_Tag = 'Actors.Perks.Change') Then
         EvaluateByPathChange(e, m, 'Perks')

         // Bookmark: Actors.Perks.Remove (TES5, SSE)
  Else If (g_Tag = 'Actors.Perks.Remove') Then
         EvaluateByPathRemove(e, m, 'Perks')

         // Bookmark: Actors.RecordFlags (!FO4)
  Else If (g_Tag = 'Actors.RecordFlags') Then
         EvaluateByPath(e, m, 'Record Header\Record Flags')

         // Bookmark: Actors.Skeleton
  Else If (g_Tag = 'Actors.Skeleton') Then
         Begin
           // assign Model elements
           x := ElementByName(e, 'Model');
           y := ElementByName(m, 'Model');

           // exit if the Model property does not exist in the control record
           If Not Assigned(x) Then
             Exit;

           // evaluate properties
           EvaluateByPath(x, y, 'MODL');
           EvaluateByPath(x, y, 'MODB');
           EvaluateByPath(x, y, 'MODT');
         End

         // Bookmark: Actors.Spells
  Else If (g_Tag = 'Actors.Spells') Then
         EvaluateByPath(e, m, 'Spells')

         // Bookmark: Actors.Stats
  Else If (g_Tag = 'Actors.Stats') Then
         Begin
           // assign DATA elements
           x := ElementBySignature(e, 'DATA');
           y := ElementBySignature(m, 'DATA');

           // evaluate CREA properties
           If sSignature = 'CREA' Then
             Begin
               EvaluateByPath(x, y, 'Health');
               EvaluateByPath(x, y, 'Combat Skill');
               EvaluateByPath(x, y, 'Magic Skill');
               EvaluateByPath(x, y, 'Stealth Skill');
               EvaluateByPath(x, y, 'Attributes');
             End

             // evaluate NPC_ properties
           Else If sSignature = 'NPC_' Then
                  Begin
                    EvaluateByPath(x, y, 'Base Health');
                    EvaluateByPath(x, y, 'Attributes');
                    EvaluateByPath(e, m, 'DNAM\Skill Values');
                    EvaluateByPath(e, m, 'DNAM\Skill Offsets');
                  End;
         End

         // Bookmark: Actors.Voice (FO3, FNV, TES5, SSE)
  Else If (g_Tag = 'Actors.Voice') Then
         EvaluateByPath(e, m, 'VTCK')

         // Bookmark: C.Acoustic
  Else If (g_Tag = 'C.Acoustic') Then
         EvaluateByPath(e, m, 'XCAS')

         // Bookmark: C.Climate
  Else If (g_Tag = 'C.Climate') Then
         Begin
           // add tag if the Behave Like Exterior flag is set ine one record but not the other
           If CompareFlags(e, m, 'DATA', 'Behave Like Exterior', True, True) Then
             Exit;

           // evaluate additional property
           EvaluateByPath(e, m, 'XCCM');
         End

         // Bookmark: C.Encounter
  Else If (g_Tag = 'C.Encounter') Then
         EvaluateByPath(e, m, 'XEZN')

         // Bookmark: C.ForceHideLand (!TES4, !FO4)
  Else If (g_Tag = 'C.ForceHideLand') Then
         EvaluateByPath(e, m, 'XCLC\Land Flags')

         // Bookmark: C.ImageSpace
  Else If (g_Tag = 'C.ImageSpace') Then
         EvaluateByPath(e, m, 'XCIM')

         // Bookmark: C.Light
  Else If (g_Tag = 'C.Light') Then
         EvaluateByPath(e, m, 'XCLL')

         // Bookmark: C.Location
  Else If (g_Tag = 'C.Location') Then
         EvaluateByPath(e, m, 'XLCN')

         // Bookmark: C.LockList
  Else If (g_Tag = 'C.LockList') Then
         EvaluateByPath(e, m, 'XILL')

         // Bookmark: C.MiscFlags (!FO4)
  Else If (g_Tag = 'C.MiscFlags') Then
         Begin
           If CompareFlags(e, m, 'DATA', 'Is Interior Cell', True, True) Then
             Exit;

           If CompareFlags(e, m, 'DATA', 'Can Travel From Here', True, True) Then
             Exit;

           If Not wbIsOblivion And Not wbIsFallout4 Then
             If CompareFlags(e, m, 'DATA', 'No LOD Water', True, True) Then
               Exit;

           If wbIsOblivion Then
             If CompareFlags(e, m, 'DATA', 'Force hide land (exterior cell) / Oblivion interior (interior cell)', True, True) Then
               Exit;

           If CompareFlags(e, m, 'DATA', 'Hand Changed', True, True) Then
             Exit;
         End

         // Bookmark: C.Music
  Else If (g_Tag = 'C.Music') Then
         EvaluateByPath(e, m, 'XCMO')

         // Bookmark: FULL (C.Name, Names, SpellStats)
  Else If ContainsStr('C.Name Names SpellStats', g_Tag) Then
         EvaluateByPath(e, m, 'FULL')

         // Bookmark: C.Owner
  Else If (g_Tag = 'C.Owner') Then
         EvaluateByPath(e, m, 'Ownership')

         // Bookmark: C.RecordFlags
  Else If (g_Tag = 'C.RecordFlags') Then
         EvaluateByPath(e, m, 'Record Header\Record Flags')

         // Bookmark: C.Regions
  Else If (g_Tag = 'C.Regions') Then
         EvaluateByPath(e, m, 'XCLR')

         // Bookmark: C.SkyLighting
         // add tag if the Behave Like Exterior flag is set in one record but not the other
  Else If (g_Tag = 'C.SkyLighting') And CompareFlags(e, m, 'DATA', 'Use Sky Lighting', True, True) Then
         Exit

         // Bookmark: C.Water
  Else If (g_Tag = 'C.Water') Then
         Begin
           // add tag if Has Water flag is set in one record but not the other
           If CompareFlags(e, m, 'DATA', 'Has Water', True, True) Then
             Exit;

           // exit if Is Interior Cell is set in either record
           If CompareFlags(e, m, 'DATA', 'Is Interior Cell', False, False) Then
             Exit;

           // evaluate properties
           EvaluateByPath(e, m, 'XCLW');
           EvaluateByPath(e, m, 'XCWT');
         End

         // Bookmark: Creatures.Blood
  Else If (g_Tag = 'Creatures.Blood') Then
         Begin
           EvaluateByPath(e, m, 'NAM0');
           EvaluateByPath(e, m, 'NAM1');
         End

         // Bookmark: Creatures.Type
  Else If (g_Tag = 'Creatures.Type') Then
         EvaluateByPath(e, m, 'DATA\Type')

         // Bookmark: Deflst
  Else If (g_Tag = 'Deflst') Then
         EvaluateByPathRemove(e, m, 'FormIDs')

         // Bookmark: Destructible
  Else If (g_Tag = 'Destructible') Then
         Begin
           // assign Destructable elements
           x := ElementByName(e, 'Destructible');
           y := ElementByName(m, 'Destructible');

           If CompareAssignment(x, y) Then
             Exit;

           a := ElementBySignature(x, 'DEST');
           b := ElementBySignature(y, 'DEST');

           // evaluate Destructable properties
           EvaluateByPath(a, b, 'Health');
           EvaluateByPath(a, b, 'Count');
           EvaluateByPath(x, y, 'Stages');

           // assign Destructable flags
           If Not wbIsSkyrim Then
             Begin
               j := ElementByName(a, 'Flags');
               k := ElementByName(b, 'Flags');

               If Assigned(j) Or Assigned(k) Then
                 Begin
                   // add tag if Destructable flags exist in one record
                   If CompareAssignment(j, k) Then
                     Exit;

                   // evaluate Destructable flags
                   If CompareKeys(j, k) Then
                     Exit;
                 End;
             End;
         End

         // Bookmark: EffectStats
  Else If (g_Tag = 'EffectStats') Then
         Begin
           If wbIsOblivion Or wbIsFallout3 Or wbIsFalloutNV Then
             Begin
               EvaluateByPath(e, m, 'DATA\Flags');

               If Not wbIsFallout3 And Not wbIsFalloutNV Then
                 EvaluateByPath(e, m, 'DATA\Base cost');

               If Not wbIsOblivion Then
                 EvaluateByPath(e, m, 'DATA\Associated Item');

               If Not wbIsFallout3 And Not wbIsFalloutNV Then
                 EvaluateByPath(e, m, 'DATA\Magic School');

               EvaluateByPath(e, m, 'DATA\Resist Value');
               EvaluateByPath(e, m, 'DATA\Projectile Speed');

               If Not wbIsFallout3 And Not wbIsFalloutNV Then
                 Begin
                   EvaluateByPath(e, m, 'DATA\Constant Effect enchantment factor');
                   EvaluateByPath(e, m, 'DATA\Constant Effect barter factor');
                 End;

               If wbIsOblivion And CompareFlags(e, m, 'DATA\Flags', 'Use actor value', False, False) Then
                 EvaluateByPath(e, m, 'DATA\Assoc. Actor Value')
               Else If wbIsFallout3 Or wbIsFalloutNV Then
                      Begin
                        EvaluateByPath(e, m, 'DATA\Archtype');
                        EvaluateByPath(e, m, 'DATA\Actor Value');
                      End;
             End
           Else If wbIsSkyrim Then
                  Begin
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Flags');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Base Cost');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Associated Item');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Magic Skill');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Resist Value');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Taper Weight');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Minimum Skill Level');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Spellmaking');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Taper Curve');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Taper Duration');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Second AV Weight');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Archtype');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Actor Value');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Casting Type');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Delivery');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Second Actor Value');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Skill Usage Multiplier');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Equip Ability');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Perk to Apply');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Script Effect AI');
                  End;
         End

         // Bookmark: EnchantmentStats
  Else If (g_Tag = 'EnchantmentStats') Then
         Begin
           If wbIsOblivion Or wbIsFallout3 Or wbIsFalloutNV Then
             Begin
               EvaluateByPath(e, m, 'ENIT\Type');
               EvaluateByPath(e, m, 'ENIT\Charge Amount');
               EvaluateByPath(e, m, 'ENIT\Enchant Cost');
               EvaluateByPath(e, m, 'ENIT\Flags');
             End
           Else If wbIsSkyrim Then
                  EvaluateByPath(e, m, 'ENIT');
         End

         // Bookmark: Factions
  Else If (g_Tag = 'Factions') Then
         Begin
           // assign Factions properties
           x := ElementByName(e, 'Factions');
           y := ElementByName(m, 'Factions');

           // add tag if the Factions properties differ
           If CompareAssignment(x, y) Then
             Exit;

           // exit if the Factions property in the control record does not exist
           If Not Assigned(x) Then
             Exit;

           // evaluate Factions properties
           If CompareKeys(x, y) Then
             Exit;
         End

         // Bookmark: Graphics
  Else If (g_Tag = 'Graphics') Then
         Begin
           // evaluate Icon and Model properties
           If ContainsStr('ALCH AMMO APPA BOOK INGR KEYM LIGH MGEF MISC SGST SLGM TREE WEAP', sSignature) Then
             Begin
               EvaluateByPath(e, m, 'Icon');
               EvaluateByPath(e, m, 'Model');
             End

             // evaluate Icon properties
           Else If ContainsStr('BSGN CLAS LSCR LTEX REGN', sSignature) Then
                  EvaluateByPath(e, m, 'Icon')

                  // evaluate Model properties
           Else If ContainsStr('ACTI DOOR FLOR FURN GRAS STAT', sSignature) Then
                  EvaluateByPath(e, m, 'Model')

                  // evaluate ARMO properties
           Else If sSignature = 'ARMO' Then
                  Begin
                    // Shared
                    EvaluateByPath(e, m, 'Male world model');
                    EvaluateByPath(e, m, 'Female world model');

                    // ARMO - Oblivion
                    If wbIsOblivion Then
                      Begin
                        // evaluate Icon properties
                        EvaluateByPath(e, m, 'Icon');
                        EvaluateByPath(e, m, 'Icon 2 (female)');

                        // assign First Person Flags elements
                        x := ElementByPath(e, 'BODT\First Person Flags');
                        If Not Assigned(x) Then
                          Exit;

                        y := ElementByPath(m, 'BODT\First Person Flags');

                        // evaluate First Person Flags
                        If CompareKeys(x, y) Then
                          Exit;

                        // assign General Flags elements
                        x := ElementByPath(e, 'BODT\General Flags');
                        If Not Assigned(x) Then
                          Exit;

                        y := ElementByPath(m, 'BODT\General Flags');

                        // evaluate General Flags
                        If CompareKeys(x, y) Then
                          Exit;
                      End

                      // ARMO - FO3, FNV
                    Else If wbIsFallout3 Or wbIsFalloutNV Then
                           Begin
                             // evaluate Icon properties
                             EvaluateByPath(e, m, 'ICON');
                             EvaluateByPath(e, m, 'ICO2');

                             // assign First Person Flags elements
                             x := ElementByPath(e, 'BMDT\Biped Flags');
                             If Not Assigned(x) Then
                               Exit;

                             y := ElementByPath(m, 'BMDT\Biped Flags');

                             // evaluate First Person Flags
                             If CompareKeys(x, y) Then
                               Exit;

                             // assign General Flags elements
                             x := ElementByPath(e, 'BMDT\General Flags');
                             If Not Assigned(x) Then
                               Exit;

                             y := ElementByPath(m, 'BMDT\General Flags');

                             // evaluate General Flags
                             If CompareKeys(x, y) Then
                               Exit;
                           End

                           // ARMO - TES5
                    Else If wbIsSkyrim Then
                           Begin
                             // evaluate Icon properties
                             EvaluateByPath(e, m, 'Icon');
                             EvaluateByPath(e, m, 'Icon 2 (female)');

                             // evaluate Biped Model properties
                             EvaluateByPath(e, m, 'Male world model');
                             EvaluateByPath(e, m, 'Female world model');

                             // assign First Person Flags elements
                             x  := ElementByPath(e, 'BOD2\First Person Flags');
                             If Not Assigned(x) Then
                               Exit;

                             y := ElementByPath(m, 'BOD2\First Person Flags');

                             // evaluate First Person Flags
                             If CompareKeys(x, y) Then
                               Exit;

                             // assign General Flags elements
                             x := ElementByPath(e, 'BOD2\General Flags');
                             If Not Assigned(x) Then
                               Exit;

                             y := ElementByPath(m, 'BOD2\General Flags');

                             // evaluate General Flags
                             If CompareKeys(x, y) Then
                               Exit;
                           End;
                  End

                  // evaluate CREA properties
           Else If sSignature = 'CREA' Then
                  Begin
                    EvaluateByPath(e, m, 'NIFZ');
                    EvaluateByPath(e, m, 'NIFT');
                  End

                  // evaluate EFSH properties
           Else If sSignature = 'EFSH' Then
                  Begin
                    // evaluate Record Flags
                    x := ElementByPath(e, 'Record Header\Record Flags');
                    y := ElementByPath(m, 'Record Header\Record Flags');

                    If CompareKeys(x, y) Then
                      Exit;

                    // evaluate Icon properties
                    EvaluateByPath(e, m, 'ICON');
                    EvaluateByPath(e, m, 'ICO2');

                    // evaluate other properties
                    EvaluateByPath(e, m, 'NAM7');

                    If wbIsSkyrim Then
                      Begin
                        EvaluateByPath(e, m, 'NAM8');
                        EvaluateByPath(e, m, 'NAM9');
                      End;

                    EvaluateByPath(e, m, 'DATA');
                  End

                  // evaluate MGEF properties
           Else If wbIsSkyrim And (sSignature = 'MGEF') Then
                  Begin
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Casting Light');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Hit Shader');
                    EvaluateByPath(e, m, 'Magic Effect Data\DATA\Enchant Shader');
                  End

                  // evaluate Material property
           Else If sSignature = 'STAT' Then
                  EvaluateByPath(e, m, 'DNAM\Material');
         End

         // Bookmark: Invent.Add
  Else If (g_Tag = 'Invent.Add') Then
         EvaluateByPathAdd(e, m, 'Items')

         // Bookmark: Invent.Change - TEST
  Else If (g_Tag = 'Invent.Change') Then
         EvaluateByPathChange(e, m, 'Items')

         // Bookmark: Invent.Remove
  Else If (g_Tag = 'Invent.Remove') Then
         EvaluateByPathRemove(e, m, 'Items')

         // Bookmark: Keywords
  Else If (g_Tag = 'Keywords') Then
         Begin
           x := ElementBySignature(e, 'KWDA');
           y := ElementBySignature(m, 'KWDA');

           If CompareAssignment(x, y) Then
             Exit;

           If CompareElementCount(x, y) Then
             Exit;

           x := ElementBySignature(e, 'KSIZ');
           y := ElementBySignature(m, 'KSIZ');

           If CompareAssignment(x, y) Then
             Exit;

           If CompareEditValue(x, y) Then
             Exit;
         End

         // Bookmark: NPC.AIPackageOverrides
  Else If (g_Tag = 'NPC.AIPackageOverrides') Then
         Begin
           If wbIsSkyrim Then
             Begin
               EvaluateByPath(e, m, 'SPOR');
               EvaluateByPath(e, m, 'OCOR');
               EvaluateByPath(e, m, 'GWOR');
               EvaluateByPath(e, m, 'ECOR');
             End;
         End

         // Bookmark: NPC.AttackRace
  Else If (g_Tag = 'NPC.AttackRace') Then
         EvaluateByPath(e, m, 'ATKR')

         // Bookmark: NPC.Class
  Else If (g_Tag = 'NPC.Class') Then
         EvaluateByPath(e, m, 'CNAM')

         // Bookmark: NPC.CrimeFaction
  Else If (g_Tag = 'NPC.CrimeFaction') Then
         EvaluateByPath(e, m, 'CRIF')

         // Bookmark: NPC.DefaultOutfit
  Else If (g_Tag = 'NPC.DefaultOutfit') Then
         EvaluateByPath(e, m, 'DOFT')

         // Bookmark: NPC.Eyes
  Else If (g_Tag = 'NPC.Eyes') Then
         EvaluateByPath(e, m, 'ENAM')

         // Bookmark: NPC.FaceGen
  Else If (g_Tag = 'NPC.FaceGen') Then
         EvaluateByPath(e, m, 'FaceGen Data')

         // Bookmark: NPC.Hair
  Else If (g_Tag = 'NPC.Hair') Then
         EvaluateByPath(e, m, 'HNAM')

         // Bookmark: NPC.Race
  Else If (g_Tag = 'NPC.Race') Then
         EvaluateByPath(e, m, 'RNAM')

         // Bookmark: ObjectBounds
  Else If (g_Tag = 'ObjectBounds') Then
         EvaluateByPath(e, m, 'OBND')

         // Bookmark: Outfits.Add
  Else If (g_Tag = 'Outfits.Add') Then
         EvaluateByPathAdd(e, m, 'OTFT')

         // Bookmark: Outfits.Remove
  Else If (g_Tag = 'Outfits.Remove') Then
         EvaluateByPathRemove(e, m, 'OTFT')

         // Bookmark: R.AddSpells - DEFER: R.ChangeSpells

         // Bookmark: R.Attributes-F
  Else If (g_Tag = 'R.Attributes-F') Then
         EvaluateByPath(e, m, 'ATTR\Female')

         // Bookmark: R.Attributes-M
  Else If (g_Tag = 'R.Attributes-M') Then
         EvaluateByPath(e, m, 'ATTR\Male')

         // Bookmark: R.Body-F
  Else If (g_Tag = 'R.Body-F') Then
         EvaluateByPath(e, m, 'Body Data\Female Body Data\Parts')

         // Bookmark: R.Body-M
  Else If (g_Tag = 'R.Body-M') Then
         EvaluateByPath(e, m, 'Body Data\Male Body Data\Parts')

         // Bookmark: R.Body-Size-F
  Else If (g_Tag = 'R.Body-Size-F') Then
         Begin
           EvaluateByPath(e, m, 'DATA\Female Height');
           EvaluateByPath(e, m, 'DATA\Female Weight');
         End

         // Bookmark: R.Body-Size-M
  Else If (g_Tag = 'R.Body-Size-M') Then
         Begin
           EvaluateByPath(e, m, 'DATA\Male Height');
           EvaluateByPath(e, m, 'DATA\Male Weight');
         End

         // Bookmark: R.ChangeSpells
  Else If (g_Tag = 'R.ChangeSpells') Then
         EvaluateByPath(e, m, 'Spells')

         // Bookmark: R.Description
  Else If (g_Tag = 'R.Description') Then
         EvaluateByPath(e, m, 'DESC')

         // Bookmark: R.Ears
  Else If (g_Tag = 'R.Ears') Then
         Begin
           EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[1]');
           EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[1]');
         End

         // Bookmark: R.Eyes
  Else If (g_Tag = 'R.Eyes') Then
         EvaluateByPath(e, m, 'ENAM')

         // Bookmark: R.Hair
  Else If (g_Tag = 'R.Hair') Then
         EvaluateByPath(e, m, 'HNAM')

         // Bookmark: R.Head
  Else If (g_Tag = 'R.Head') Then
         Begin
           EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[0]');
           EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[0]');
           EvaluateByPath(e, m, 'FaceGen Data');
         End

         // Bookmark: R.Mouth
  Else If (g_Tag = 'R.Mouth') Then
         Begin
           EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[2]');
           EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[2]');
         End

         // Bookmark: R.Relations.Add
  Else If (g_Tag = 'R.Relations.Add') Then
         EvaluateByPathAdd(e, m, 'Relations')

         // Bookmark: R.Relations.Change - TEST
  Else If (g_Tag = 'R.Relations.Change') Then
         EvaluateByPathChange(e, m, 'Relations')

         // Bookmark: R.Relations.Remove
  Else If (g_Tag = 'R.Relations.Remove') Then
         EvaluateByPathRemove(e, m, 'Relations')

         // Bookmark: R.Skills
  Else If (g_Tag = 'R.Skills') Then
         EvaluateByPath(e, m, 'DATA\Skill Boosts')

         // Bookmark: R.Teeth
  Else If (g_Tag = 'R.Teeth') Then
         Begin
           EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[3]');
           EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[3]');

           // FO3
           If wbIsFallout3 Then
             Begin
               EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[4]');
               EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[4]');
             End;
         End

         // Bookmark: R.Voice-F
  Else If (g_Tag = 'R.Voice-F') Then
         EvaluateByPath(e, m, 'VTCK\Voice #1 (Female)')

         // Bookmark: R.Voice-M
  Else If (g_Tag = 'R.Voice-M') Then
         EvaluateByPath(e, m, 'VTCK\Voice #0 (Male)')

         // Bookmark: Relations.Add
  Else If (g_Tag = 'Relations.Add') Then
         EvaluateByPathAdd(e, m, 'Relations')

         // Bookmark: Relations.Change - TEST
  Else If (g_Tag = 'Relations.Change') Then
         EvaluateByPathChange(e, m, 'Relations')

         // Bookmark: Relations.Remove
  Else If (g_Tag = 'Relations.Remove') Then
         EvaluateByPathRemove(e, m, 'Relations')

         // Bookmark: Roads
  Else If (g_Tag = 'Roads') Then
         EvaluateByPath(e, m, 'PGRP')

         // Bookmark: Scripts
  Else If (g_Tag = 'Scripts') Then
         EvaluateByPath(e, m, 'SCRI')

         // Bookmark: Sound
  Else If (g_Tag = 'Sound') Then
         Begin
           // Activators, Containers, Doors, and Lights
           If ContainsStr('ACTI CONT DOOR LIGH', sSignature) Then
             Begin
               EvaluateByPath(e, m, 'SNAM');

               // Activators
               If sSignature = 'ACTI' Then
                 EvaluateByPath(e, m, 'VNAM')

                 // Containers
               Else If sSignature = 'CONT' Then
                      Begin
                        EvaluateByPath(e, m, 'QNAM');
                        If Not wbIsSkyrim And Not wbIsFallout3 Then
                          EvaluateByPath(e, m, 'RNAM');
                        // FO3, TESV, and SSE don't have this element
                      End

                      // Doors
               Else If sSignature = 'DOOR' Then
                      Begin
                        EvaluateByPath(e, m, 'ANAM');
                        EvaluateByPath(e, m, 'BNAM');
                      End;
             End

             // Creatures
           Else If sSignature = 'CREA' Then
                  Begin
                    EvaluateByPath(e, m, 'WNAM');
                    EvaluateByPath(e, m, 'CSCR');
                    EvaluateByPath(e, m, 'Sound Types');
                  End

                  // Magic Effects
           Else If sSignature = 'MGEF' Then
                  Begin
                    // TES5, SSE
                    If wbIsSkyrim Then
                      EvaluateByPath(e, m, 'SNDD')

                      // FO3, FNV, TES4
                    Else
                      Begin
                        EvaluateByPath(e, m, 'DATA\Effect sound');
                        EvaluateByPath(e, m, 'DATA\Bolt sound');
                        EvaluateByPath(e, m, 'DATA\Hit sound');
                        EvaluateByPath(e, m, 'DATA\Area sound');
                      End;
                  End

                  // Weather
           Else If sSignature = 'WTHR' Then
                  EvaluateByPath(e, m, 'Sounds');
         End

         // Bookmark: SpellStats
  Else If (g_Tag = 'SpellStats') Then
         EvaluateByPath(e, m, 'SPIT')

         // Bookmark: Stats
  Else If (g_Tag = 'Stats') Then
         Begin
           If ContainsStr('ALCH AMMO APPA ARMO BOOK CLOT INGR KEYM LIGH MISC SGST SLGM WEAP', sSignature) Then
             Begin
               EvaluateByPath(e, m, 'EDID');
               EvaluateByPath(e, m, 'DATA');

               If ContainsStr('ARMO WEAP', sSignature) Then
                 EvaluateByPath(e, m, 'DNAM')

               Else If sSignature = 'WEAP' Then
                      EvaluateByPath(e, m, 'CRDT');
             End

           Else If sSignature = 'ARMA' Then
                  EvaluateByPath(e, m, 'DNAM');
         End

         // Bookmark: Text
  Else If (g_Tag = 'Text') Then
         Begin
           If ContainsStr('ALCH AMMO APPA ARMO AVIF BOOK BSGN CHAL CLAS IMOD LSCR MESG MGEF PERK SCRL SHOU SKIL SPEL TERM WEAP', sSignature) Then
             EvaluateByPath(e, m, 'DESC')

           Else If Not wbIsOblivion Then
                  Begin
                    If sSignature = 'BOOK' Then
                      EvaluateByPath(e, m, 'CNAM')

                    Else If sSignature = 'MGEF' Then
                           EvaluateByPath(e, m, 'DNAM')

                    Else If sSignature = 'NOTE' Then
                           EvaluateByPath(e, m, 'TNAM');
                  End;
         End

         // Bookmark: WeaponMods
  Else If (g_Tag = 'WeaponMods') Then
         EvaluateByPath(e, m, 'Weapon Mods');
End;


Procedure ProcessDelevRelevTags(ARecord: IwbMainRecord; AMaster: IwbMainRecord);

Var 
  kEntries          : IwbElement;
  kEntriesMaster    : IwbElement;
  kEntry            : IwbElement;
  kEntryMaster      : IwbElement;
  kCOED             : IwbElement;
  // extra data
  kCOEDMaster       : IwbElement;
  // extra data
  sSignature        : string;
  sEditValues       : string;
  sMasterEditValues : string;
  i                 : integer;
  j                 : integer;
Begin
  // nothing to do if already tagged
  If TagExists('Delev') And TagExists('Relev') Then
    Exit;

  // get Leveled List Entries
  kEntries       := ElementByName(ARecord, 'Leveled List Entries');
  kEntriesMaster := ElementByName(AMaster, 'Leveled List Entries');

  If Not Assigned(kEntries) Then
    Exit;

  If Not Assigned(kEntriesMaster) Then
    Exit;

  // initalize count matched on reference entries
  j := 0;

  If Not TagExists('Relev') Then
    Begin
      g_Tag := 'Relev';

      For i := 0 To Pred(ElementCount(kEntries)) Do
        Begin
          kEntry := ElementByIndex(kEntries, i);
          kEntryMaster := SortedArrayElementByValue(kEntriesMaster, 'LVLO\Reference', GetElementEditValues(kEntry, 'LVLO\Reference'));

          If Not Assigned(kEntryMaster) Then
            Continue;

          Inc(j);

          If TagExists(g_Tag) Then
            Continue;

          If CompareNativeValues(kEntry, kEntryMaster, 'LVLO\Level') Then
            Exit;

          If CompareNativeValues(kEntry, kEntryMaster, 'LVLO\Count') Then
            Exit;

          If wbIsOblivion Then
            Continue;

          // Relev check for changed level, count, extra data
          kCOED       := ElementBySignature(kEntry, 'COED');
          kCOEDMaster := ElementBySignature(kEntryMaster, 'COED');

          sEditValues       := EditValues(kCOED);
          sMasterEditValues := EditValues(kCOEDMaster);

          If Not SameText(sEditValues, sMasterEditValues) Then
            Begin
              AddLogEntry('Assigned', kCOED, kCOEDMaster);
              slSuggestedTags.Add(g_Tag);
              Exit;
            End;
        End;
    End;

  If Not TagExists('Delev') Then
    Begin
      g_Tag := 'Delev';

      sSignature := Signature(ARecord);

      If (((sSignature = 'LVLC') And (wbIsOblivion Or wbIsFallout3 Or wbIsFalloutNV))
         Or (sSignature = 'LVLI') Or ((sSignature = 'LVLN') And Not wbIsOblivion)
         Or ((sSignature = 'LVSP') And (wbIsOblivion Or wbIsSkyrim)))
         And Not TagExists(g_Tag) Then
        // if number of matched entries less than in master list
        If j < ElementCount(kEntriesMaster) Then
          Begin
            AddLogEntry('ElementCount', kEntries, kEntriesMaster);
            slSuggestedTags.Add(g_Tag);
            Exit;
          End;
    End;
End;


Function AddLogEntry(ATestName: String; AElement: IwbElement; AMaster: IwbElement): string;

Var 
  mr    : IwbMainRecord;
  sName : string;
  sPath : string;
Begin
  If Not g_LogTests Then
    Exit;

  If Assigned(AMaster) Then
    Begin
      mr    := ContainingMainRecord(AMaster);
      sPath := Path(AMaster);
    End
  Else
    Begin
      mr    := ContainingMainRecord(AElement);
      sPath := Path(AElement);
    End;

  sPath := RightStr(sPath, Length(sPath) - 5);

  sName := Format('[%s:%s]', [Signature(mr), IntToHex(GetLoadOrderFormID(mr), 8)]);

  slLog.Add(Format('{%s} (%s) %s %s', [g_Tag, ATestName, sName, sPath]));
End;


Function FileByName(AFileName: String): IwbFile;

Var 
  kFile : IwbFile;
  i     : integer;
Begin
  Result := Nil;

  For i := 0 To Pred(FileCount) Do
    Begin
      kFile := FileByIndex(i);
      If SameText(AFileName, GetFileName(kFile)) Then
        Begin
          Result := kFile;
          Exit;
        End;
    End;
End;


Procedure EscKeyHandler(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If Key = 27 Then
    Sender.Close;
End;


Procedure chkAddTagsClick(Sender: TObject);
Begin
  g_AddTags := Sender.Checked;
End;

Procedure chkAddFileClick(Sender: TObject);
Begin
  g_AddFile := Sender.Checked;
End;


Procedure chkLoggingClick(Sender: TObject);
Begin
  g_LogTests := Sender.Checked;
End;


Function ShowPrompt(ACaption: String): integer;

Var 
  frm        : TForm;
  chkAddTags : TCheckBox;
  chkAddFile : TCheckBox;
  chkLogging : TCheckBox;
  btnCancel  : TButton;
  btnOk      : TButton;
  i          : integer;
Begin
  Result := mrCancel;

  frm := TForm.Create(TForm(frmMain));

  Try
    frm.Caption      := ACaption;
    frm.BorderStyle  := bsToolWindow;
    frm.ClientWidth  := 234 * ScaleFactor;
    frm.ClientHeight := 157 * ScaleFactor;
    frm.Position     := poScreenCenter;
    frm.KeyPreview   := True;
    frm.OnKeyDown    := EscKeyHandler;

    chkAddTags := TCheckBox.Create(frm);
    chkAddTags.Parent   := frm;
    chkAddTags.Left     := 16 * ScaleFactor;
    chkAddTags.Top      := 16 * ScaleFactor;
    chkAddTags.Width    := 185 * ScaleFactor;
    chkAddTags.Height   := 16 * ScaleFactor;
    chkAddTags.Caption  := 'Write suggested tags to header';
    chkAddTags.Checked  := True;
    g_AddTags := chkAddTags.Checked;
    chkAddTags.OnClick  := chkAddTagsClick;
    chkAddTags.TabOrder := 0;

    chkAddFile := TCheckBox.Create(frm);
    chkAddFile.Parent   := frm;
    chkAddFile.Left     := 16 * ScaleFactor;
    chkAddFile.Top      := 39 * ScaleFactor;
    chkAddFile.Width    := 185 * ScaleFactor;
    chkAddFile.Height   := 16 * ScaleFactor;
    chkAddFile.Caption  := 'Write suggested tags to file';
    chkAddFile.Checked  := False;
    g_AddFile := chkAddFile.Checked;
    chkAddFile.OnClick  := chkAddFileClick;
    chkAddFile.TabOrder := 0;

    chkLogging := TCheckBox.Create(frm);
    chkLogging.Parent   := frm;
    chkLogging.Left     := 16 * ScaleFactor;
    chkLogging.Top      := 62 * ScaleFactor;
    chkLogging.Width    := 185 * ScaleFactor;
    chkLogging.Height   := 16 * ScaleFactor;
    chkLogging.Caption  := 'Log test results to Messages tab';
    chkLogging.Checked  := True;
    g_LogTests := chkLogging.Checked;
    chkLogging.OnClick  := chkLoggingClick;
    chkLogging.TabOrder := 1;

    btnOk := TButton.Create(frm);
    btnOk.Parent              := frm;
    btnOk.Left                := 62 * ScaleFactor;
    btnOk.Top                 := 123 * ScaleFactor;
    btnOk.Width               := 75 * ScaleFactor;
    btnOk.Height              := 25 * ScaleFactor;
    btnOk.Caption             := 'Run';
    btnOk.Default             := True;
    btnOk.ModalResult         := mrOk;
    btnOk.TabOrder            := 3;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent          := frm;
    btnCancel.Left            := 143 * ScaleFactor;
    btnCancel.Top             := 123 * ScaleFactor;
    btnCancel.Width           := 75 * ScaleFactor;
    btnCancel.Height          := 25 * ScaleFactor;
    btnCancel.Caption         := 'Abort';
    btnCancel.ModalResult     := mrAbort;
    btnCancel.TabOrder        := 4;

    Result := frm.ShowModal;
  Finally
    frm.Free;
  End;
End;

End.
