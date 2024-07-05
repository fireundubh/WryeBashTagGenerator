{
  Generates bash tags for a selected plugin automatically

  Games:  FO3/FNV/FO4/TES4/TES5/SSE/Enderal/EnderalSE
  Author: fireundubh <fireundubh@gmail.com>
  Hotkey: F12
}

unit WryeBashTagGenerator;


const
  ScriptName    = 'WryeBashTagGenerator';
  ScriptVersion = '1.6.4.7';
  ScriptAuthor  = 'fireundubh';
  ScriptEmail   = 'fireundubh@gmail.com';
  ScaleFactor   = Screen.PixelsPerInch / 96;


var
  slBadTags        : TStringList;
  slDifferentTags  : TStringList;
  slExistingTags   : TStringList;
  slLog            : TStringList;
  slSuggestedTags  : TStringList;
  slDeprecatedTags : TStringList;
  g_FileName       : string;
  g_Tag            : string;
  g_AddTags        : boolean;
  g_LogTests       : boolean;


function wbIsOblivion: boolean;
begin
  Result := wbGameMode = 1;
end;


function wbIsSkyrim: boolean;
begin
  Result := (wbGameMode = 4) or (wbGameMode = 5) or (wbGameMode = 7) or (wbGameMode = 8) or (wbGameMode = 9);
end;


function wbIsSkyrimSE: boolean;
begin
  Result := (wbGameMode = 7) or (wbGameMode = 9);
end;


function wbIsFallout3: boolean;
begin
  Result := wbGameMode = 2;
end;


function wbIsFalloutNV: boolean;
begin
  Result := wbGameMode = 3;
end;


function wbIsFallout4: boolean;
begin
  Result := (wbGameMode = 6) or (wbGameMode = 10);
end;


function wbIsFallout76: boolean;
begin
  Result := wbGameMode = 11;
end;


function wbIsEnderal: boolean;
begin
  Result := wbGameMode = 5;
end;


function wbIsEnderalSE: boolean;
begin
  Result := wbGameMode = 9;
end;


procedure LogInfo(AText: string);
begin
  AddMessage('[INFO] ' + AText);
end;


procedure LogWarn(AText: string);
begin
  AddMessage('[WARN] ' + AText);
end;


procedure LogError(AText: string);
begin
  AddMessage('[ERRO] ' + AText);
end;


function Initialize: integer;
begin
  ClearMessages();

  LogInfo('--------------------------------------------------------------------------------');
  LogInfo(ScriptName + ' v' + ScriptVersion + ' by ' + ScriptAuthor + ' <' + ScriptEmail + '>');
  LogInfo('--------------------------------------------------------------------------------');

  g_AddTags  := False;
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

  if ShowPrompt(ScriptName + ' v' + ScriptVersion) = mrAbort then
  begin
    LogError('Cannot proceed because user aborted execution');
    Result := 1;
    Exit;
  end;

  if wbIsFallout76 then
  begin
    LogError('Cannot proceed because CBash does not support Fallout 76');
    Result := 2;
    Exit;
  end;

  if wbIsFallout3 then
    LogInfo('Using game mode: Fallout 3')
  else if wbIsFalloutNV then
    LogInfo('Using game mode: Fallout: New Vegas')
  else if wbIsFallout4 then
    LogInfo('Using game mode: Fallout 4')
  else if wbIsOblivion then
    LogInfo('Using game mode: Oblivion')
  else if wbIsEnderal then
    LogInfo('Using game mode: Enderal')
  else if wbIsEnderalSE then
    LogInfo('Using game mode: Enderal Special Edition')
  else if wbIsSkyrimSE then
    LogInfo('Using game mode: Skyrim Special Edition')
  else if wbIsSkyrim then
    LogInfo('Using game mode: Skyrim')
  else
  begin
    LogError('Cannot proceed because script does not support game mode');
    Result := 3;
    Exit;
  end;

  ProcessFile(FileByIndex(Pred(FileCount)));
end;


function ProcessFile(f: IwbFile): integer;
var
  kDescription : IwbElement;
  kHeader      : IwbElement;
  sDescription : string;
  sTags        : string;
  sMasterName  : string;
  r            : IwbMainRecord;
  i            : integer;
begin
  g_FileName := GetFileName(f);

  AddMessage(#10);

  LogInfo('Processing... ' + IntToStr(RecordCount(f)) + ' records. Please wait. This could take a while.');

  for i := 0 to Pred(RecordCount(f)) do
    ProcessRecord(RecordByIndex(f, i));

  LogInfo('--------------------------------------------------------------------------------');
  LogInfo(g_FileName);
  LogInfo('-------------------------------------------------------------------------- TESTS');

  if g_LogTests then
    for i := 0 to Pred(slLog.Count) do
      LogInfo(slLog[i]);

  LogInfo('------------------------------------------------------------------------ RESULTS');

  if slSuggestedTags.Count > 0 then
  begin
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

    if SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) then
    begin
      LogInfo(FormatTags(slExistingTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.'));
      LogInfo(FormatTags(slSuggestedTags, 'suggested tag:', 'suggested tags:', 'No suggested tags.'));
      LogWarn('No tags to add.' + #13#10);
      Exit;
    end;

    if g_AddTags then
    begin
      // if the description element doesn't exist, add the element
      kDescription := ElementBySignature(kHeader, 'SNAM');
      if not Assigned(kDescription) then
        kDescription := Add(kHeader, 'SNAM', True);

      sDescription := GetEditValue(kDescription);
      sTags        := Format('{{BASH:%s}}', [slSuggestedTags.DelimitedText]);

      if (Length(sDescription) = 0) and (slSuggestedTags.Count > 0) then
        sDescription := sTags
      else if not SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) then
      begin
        if slExistingTags.Count = 0 then
          sDescription := sDescription + #10#10 +sTags
        else
          sDescription := RegExReplace('{{BASH:.*?}}', sTags, sDescription);
      end;

      SetEditValue(kDescription, sDescription);

      LogInfo(FormatTags(slBadTags,       'bad tag removed:',          'bad tags removed:',          'No bad tags found.'));
      LogInfo(FormatTags(slDifferentTags, 'tag added to file header:', 'tags added to file header:', 'No tags added.'));
    end
    else
    begin
      LogInfo(FormatTags(slExistingTags,  'existing tag found:',    'existing tags found:',    'No existing tags found.'));
      LogInfo(FormatTags(slBadTags,       'bad tag found:',         'bad tags found:',         'No bad tags found.'));
      LogInfo(FormatTags(slDifferentTags, 'suggested tag to add:',  'suggested tags to add:',  'No suggested tags to add.'));
      LogInfo(FormatTags(slSuggestedTags, 'suggested tag overall:', 'suggested tags overall:', 'No suggested tags overall.'));
    end;
  end else
    LogInfo('No tags are suggested for this plugin.');

  slLog.Clear;
  slSuggestedTags.Clear;
  slExistingTags.Clear;
  slDifferentTags.Clear;
  slBadTags.Clear;

  AddMessage(#10);
end;


function ProcessRecord(e: IwbMainRecord): integer;
var
  o             : IwbMainRecord;
  sSignature    : string;
  ConflictState : TConflictThis;
  iFormID       : integer;
begin
  ConflictState := ConflictAllForMainRecord(e);

  if (ConflictState = caUnknown)
  or (ConflictState = caOnlyOne)
  or (ConflictState = caNoConflict) then
    Exit;

  // exit if the record should not be processed
  if SameText(g_FileName, 'Dawnguard.esm') then
  begin
    iFormID := GetLoadOrderFormID(e) and $00FFFFFF;
    if (iFormID = $00016BCF)
    or (iFormID = $0001EE6D)
    or (iFormID = $0001FA4C)
    or (iFormID = $00039F67)
    or (iFormID = $0006C3B6) then
      Exit;
  end;

  // get master record if record is an override
  o := Master(e);

  if not Assigned(o) then
    Exit;

  // if record overrides several masters, then get the last one
  o := HighestOverrideOrSelf(o, OverrideCount(o));

  if Equals(e, o) then
    Exit;

  // stop processing deleted records to avoid errors
  if GetIsDeleted(e)
  or GetIsDeleted(o) then
    Exit;

  sSignature := Signature(e);

  LogInfo(Name(e));

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FNV
  // -------------------------------------------------------------------------------
  if wbIsFalloutNV then
    if sSignature = 'WEAP' then
      ProcessTag('WeaponMods', e, o);

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to TES4
  // -------------------------------------------------------------------------------
  if wbIsOblivion then
  begin
    if ContainsStr('CREA NPC_', sSignature) then
    begin
      ProcessTag('Actors.Spells', e, o);

      if sSignature = 'CREA' then
        ProcessTag('Creatures.Blood', e, o);
    end

    else if sSignature = 'RACE' then
    begin
      ProcessTag('R.ChangeSpells', e, o);
      ProcessTag('R.Attributes-F', e, o);
      ProcessTag('R.Attributes-M', e, o);
    end

    else if sSignature = 'ROAD' then
      ProcessTag('Roads', e, o)

    else if sSignature = 'SPEL' then
      ProcessTag('SpellStats', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to TES5, SSE
  // -------------------------------------------------------------------------------
  if wbIsSkyrim then
  begin
    if sSignature = 'CELL' then
    begin
      ProcessTag('C.Location', e, o);
      ProcessTag('C.LockList', e, o);
      ProcessTag('C.Regions', e, o);
      ProcessTag('C.SkyLighting', e, o);
    end

    else if ContainsStr('ACTI ALCH AMMO ARMO BOOK FLOR FURN INGR KEYM LCTN MGEF MISC NPC_ SCRL SLGM SPEL TACT WEAP', sSignature) then
      ProcessTag('Keywords', e, o)

    else if sSignature = 'FACT' then
    begin
      ProcessTag('Relations.Add', e, o);
      ProcessTag('Relations.Change', e, o);
      ProcessTag('Relations.Remove', e, o);
    end

    else if sSignature = 'NPC_' then
    begin
      ProcessTag('Actors.Perks.Add', e, o);
      ProcessTag('Actors.Perks.Change', e, o);
      ProcessTag('Actors.Perks.Remove', e, o);
      ProcessTag('Factions', e, o);

      g_Tag := 'NPC.AIPackageOverrides';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) then
        ProcessTag('NPC.AIPackageOverrides', e, o);

      ProcessTag('NPC.AttackRace', e, o);
      ProcessTag('NPC.CrimeFaction', e, o);
      ProcessTag('NPC.DefaultOutfit', e, o);
    end

    else if sSignature = 'OTFT' then
    begin
      ProcessTag('Outfits.Add', e, o);
      ProcessTag('Outfits.Remove', e, o);
    end;
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV
  // -------------------------------------------------------------------------------
  if wbIsFallout3 or wbIsFalloutNV then
  begin
    if sSignature = 'FLST' then
      ProcessTag('Deflst', e, o);

    g_Tag := 'Destructible';
    if ContainsStr('ACTI ALCH AMMO BOOK CONT DOOR FURN IMOD KEYM MISC MSTT PROJ TACT TERM WEAP', sSignature) then
      ProcessTag('Destructible', e, o)

    // special handling for CREA and NPC_ record types
    else if ContainsStr('CREA NPC_', sSignature) then
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
        ProcessTag('Destructible', e, o)

    // added in Wrye Bash 307 Beta 6
    else if sSignature = 'FACT' then
    begin
      ProcessTag('Relations.Add', e, o);
      ProcessTag('Relations.Change', e, o);
      ProcessTag('Relations.Remove', e, o);
    end;
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES4
  // -------------------------------------------------------------------------------
  if wbIsFallout3 or wbIsFalloutNV or wbIsOblivion then
  begin
    if ContainsStr('CREA NPC_', sSignature) then
    begin
      if sSignature = 'CREA' then
        ProcessTag('Creatures.Type', e, o);

      g_Tag := 'Factions';
      if wbIsOblivion or not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Factions', False, False) then
        ProcessTag('Factions', e, o);

      if sSignature = 'NPC_' then
      begin
        ProcessTag('NPC.Eyes', e, o);
        ProcessTag('NPC.FaceGen', e, o);
        ProcessTag('NPC.Hair', e, o);
      end;
    end

    else if sSignature = 'RACE' then
    begin
      ProcessTag('R.Body-F', e, o);
      ProcessTag('R.Body-M', e, o);
      ProcessTag('R.Body-Size-F', e, o);
      ProcessTag('R.Body-Size-M', e, o);
      ProcessTag('R.Eyes', e, o);
      ProcessTag('R.Hair', e, o);
      ProcessTag('R.Relations.Add', e, o);
      ProcessTag('R.Relations.Change', e, o);
      ProcessTag('R.Relations.Remove', e, o);
    end;
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES5, SSE
  // -------------------------------------------------------------------------------
  if wbIsFallout3 or wbIsFalloutNV or wbIsSkyrim then
  begin
    if ContainsStr('CREA NPC_', sSignature) then
    begin
      g_Tag := 'Actors.ACBS';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
        ProcessTag('Actors.ACBS', e, o);

      g_Tag := 'Actors.AIData';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Data', False, False) then
        ProcessTag('Actors.AIData', e, o);

      g_Tag := 'Actors.AIPackages';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) then
        ProcessTag('Actors.AIPackages', e, o);

      if sSignature = 'CREA' then
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
          ProcessTag('Actors.Anims', e, o);

      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
      begin
        ProcessTag('Actors.CombatStyle', e, o);
        ProcessTag('Actors.DeathItem', e, o);
      end;

      g_Tag := 'Actors.Skeleton';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
        ProcessTag('Actors.Skeleton', e, o);

      g_Tag := 'Actors.Stats';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
        ProcessTag('Actors.Stats', e, o);

      if wbIsFallout3 or wbIsFalloutNV or (sSignature = 'NPC_') then
        ProcessTag('Actors.Voice', e, o);

      if sSignature = 'NPC_' then
      begin
        g_Tag := 'NPC.Class';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
          ProcessTag('NPC.Class', e, o);

        g_Tag := 'NPC.Race';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
          ProcessTag('NPC.Race', e, o);
      end;

      g_Tag := 'Scripts';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Script', False, False) then
        ProcessTag(g_Tag, e, o);
    end;

    if sSignature = 'CELL' then
    begin
      ProcessTag('C.Acoustic', e, o);
      ProcessTag('C.Encounter', e, o);
      ProcessTag('C.ForceHideLand', e, o);
      ProcessTag('C.ImageSpace', e, o);
    end;

    if sSignature = 'RACE' then
    begin
      ProcessTag('R.Ears', e, o);
      ProcessTag('R.Head', e, o);
      ProcessTag('R.Mouth', e, o);
      ProcessTag('R.Teeth', e, o);
      ProcessTag('R.Skills', e, o);
      ProcessTag('R.Description', e, o);
      ProcessTag('Voice-F', e, o);
      ProcessTag('Voice-M', e, o);
    end;

    if ContainsStr('ACTI ALCH ARMO CONT DOOR FLOR FURN INGR KEYM LIGH LVLC MISC QUST WEAP', sSignature) then
      ProcessTag('Scripts', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES4, TES5, SSE
  // -------------------------------------------------------------------------------
  if wbIsFallout3 or wbIsFalloutNV or wbIsOblivion or wbIsSkyrim then
  begin
    if sSignature = 'CELL' then
    begin
      ProcessTag('C.Climate', e, o);
      ProcessTag('C.Light', e, o);
      ProcessTag('C.MiscFlags', e, o);
      ProcessTag('C.Music', e, o);
      ProcessTag('C.Name', e, o);
      ProcessTag('C.Owner', e, o);
      ProcessTag('C.RecordFlags', e, o);
      ProcessTag('C.Water', e, o);
    end;

    // TAG: Delev, Relev
    if ContainsStr('LVLC LVLI LVLN LVSP', sSignature) then
      ProcessDelevRelevTags(e, o);

    if ContainsStr('ACTI ALCH AMMO APPA ARMO BOOK BSGN CLAS CLOT DOOR FLOR FURN INGR KEYM LIGH MGEF MISC SGST SLGM WEAP', sSignature) then
    begin
      ProcessTag('Graphics', e, o);
      ProcessTag('Names', e, o);
      ProcessTag('Stats', e, o);

      if ContainsStr('ACTI DOOR LIGH MGEF', sSignature) then
      begin
        ProcessTag('Sound', e, o);

        if sSignature = 'MGEF' then
          ProcessTag('EffectStats', e, o);
      end;
    end;

    if ContainsStr('CREA EFSH GRAS LSCR LTEX REGN STAT TREE', sSignature) then
      ProcessTag('Graphics', e, o);

    if sSignature = 'CONT' then
    begin
      ProcessTag('Invent.Add', e, o);
      ProcessTag('Invent.Change', e, o);
      ProcessTag('Invent.Remove', e, o);
      ProcessTag('Names', e, o);
      ProcessTag('Sound', e, o);
    end;

    if ContainsStr('DIAL ENCH EYES FACT HAIR QUST RACE SPEL WRLD', sSignature) then
    begin
      ProcessTag('Names', e, o);

      if sSignature = 'ENCH' then
        ProcessTag('EnchantmentStats', e, o);

      if sSignature = 'SPEL' then
        ProcessTag('SpellStats', e, o);
    end;

    if sSignature = 'FACT' then
    begin
      ProcessTag('Relations.Add', e, o);
      ProcessTag('Relations.Change', e, o);
      ProcessTag('Relations.Remove', e, o);
    end;

    if (sSignature = 'WTHR') then
      ProcessTag('Sound', e, o);

    // special handling for CREA and NPC_
    if ContainsStr('CREA NPC_', sSignature) then
    begin
      if wbIsOblivion or wbIsFallout3 or wbIsFalloutNV or (sSignature = 'NPC_') then
        ProcessTag('Actors.RecordFlags', e, o);

      if wbIsOblivion then
      begin
        ProcessTag('Invent.Add', e, o);
        ProcessTag('Invent.Change', e, o);
        ProcessTag('Invent.Remove', e, o);
        ProcessTag('Names', e, o);

        if sSignature = 'CREA' then
          ProcessTag('Sound', e, o);
      end;

      if not wbIsOblivion then
      begin
        g_Tag := 'Invent.Add';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(g_Tag, e, o);

        g_Tag := 'Invent.Change';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(g_Tag, e, o);

        g_Tag := 'Invent.Remove';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(g_Tag, e, o);

        // special handling for CREA and NPC_ record types
        g_Tag := 'Names';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Base Data', False, False) then
          ProcessTag(g_Tag, e, o);

        // special handling for CREA record type
        g_Tag := 'Sound';
        if sSignature = 'CREA' then
          if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
            ProcessTag(g_Tag, e, o);
      end;
    end;
  end;

  // ObjectBounds
  g_Tag := 'ObjectBounds';

  if wbIsFallout3 and ContainsStr('ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK COBJ CONT CREA DOOR EXPL FURN GRAS IDLM INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP', sSignature) then
    ProcessTag(g_Tag, e, o);

  if wbIsFalloutNV and ContainsStr('ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK CCRD CHIP CMNY COBJ CONT CREA DOOR EXPL FURN GRAS IDLM IMOD INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP', sSignature) then
    ProcessTag(g_Tag, e, o);

  if wbIsSkyrim and ContainsStr('ACTI ADDN ALCH AMMO APPA ARMO ARTO ASPC BOOK CONT DOOR DUAL ENCH EXPL FLOR FURN GRAS HAZD IDLM INGR KEYM LIGH LVLI LVLN LVSP MISC MSTT NPC_ PROJ SCRL SLGM SOUN SPEL STAT TACT TREE TXST WEAP', sSignature) then
    ProcessTag(g_Tag, e, o);

  if wbIsFallout4 and ContainsStr('LVLI LVLN', sSignature) then
    ProcessTag(g_Tag, e, o);

  // Text
  if not wbIsFallout4 then
  begin
    g_Tag := 'Text';

    if wbIsOblivion and ContainsStr('BOOK BSGN CLAS LSCR MGEF SKIL', sSignature) then
      ProcessTag(g_Tag, e, o);

    if wbIsFallout3 and ContainsStr('AVIF BOOK CLAS LSCR MESG MGEF NOTE PERK TERM', sSignature) then
      ProcessTag(g_Tag, e, o);

    if wbIsFalloutNV and ContainsStr('AVIF BOOK CHAL CLAS IMOD LSCR MESG MGEF NOTE PERK TERM', sSignature) then
      ProcessTag(g_Tag, e, o);

    if wbIsSkyrim and ContainsStr('ALCH AMMO APPA ARMO AVIF BOOK CLAS LSCR MESG MGEF SCRL SHOU SPEL WEAP', sSignature) then
      ProcessTag(g_Tag, e, o);
  end;
end;


function Finalize: integer;
begin
  slLog.Free;
  slSuggestedTags.Free;
  slExistingTags.Free;
  slDifferentTags.Free;
  slBadTags.Free;
  slDeprecatedTags.Free;
end;


function StrToBool(AValue: string): boolean;
begin
  if (AValue <> '0') and (AValue <> '1') then
    Result := nil
  else
    Result := (AValue = '1');
end;


function RegExMatchGroup(AExpr: string; ASubj: string; AGroup: integer): string;
var
  re     : TPerlRegEx;
begin
  Result := '';
  re := TPerlRegEx.Create;
  try
    re.RegEx := AExpr;
    re.Options := [];
    re.Subject := ASubj;
    if re.Match then
      Result := re.Groups[AGroup];
  finally
    re.Free;
  end;
end;


function RegExReplace(const AExpr: string; ARepl: string; ASubj: string): string;
var
  re      : TPerlRegEx;
  sResult : string;
begin
  Result := '';
  re := TPerlRegEx.Create;
  try
    re.RegEx := AExpr;
    re.Options := [];
    re.Subject := ASubj;
    re.Replacement := ARepl;
    re.ReplaceAll;
    sResult := re.Subject;
  finally
    re.Free;
    Result := sResult;
  end;
end;


function EditValues(const AElement: IwbElement): string;
var
  kElement : IInterface;
  sName    : string;
  i        : integer;
begin
  Result := GetEditValue(AElement);

  for i := 0 to Pred(ElementCount(AElement)) do
  begin
    kElement := ElementByIndex(AElement, i);
    sName    := Name(kElement);

    if SameText(sName, 'unknown') or SameText(sName, 'unused') then
      Continue;

    if Result <> '' then
      Result := Result + ' ' + EditValues(kElement)
    else
      Result := EditValues(kElement);
  end;
end;


function CompareAssignment(AElement: IwbElement; AMaster: IwbElement): boolean;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  if not Assigned(AElement) and not Assigned(AMaster) then
    Exit;

  if Assigned(AElement) and Assigned(AMaster) then
    Exit;

  AddLogEntry('Assigned', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function CompareElementCount(AElement: IwbElement; AMaster: IwbElement): boolean;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  if ElementCount(AElement) = ElementCount(AMaster) then
    Exit;

  AddLogEntry('ElementCount', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function CompareElementCountAdd(AElement: IwbElement; AMaster: IwbElement): boolean;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  if ElementCount(AElement) <= ElementCount(AMaster) then
    Exit;

  AddLogEntry('ElementCountAdd', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function CompareElementCountRemove(AElement: IwbElement; AMaster: IwbElement): boolean;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  if ElementCount(AElement) >= ElementCount(AMaster) then
    Exit;

  AddLogEntry('ElementCountRemove', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function CompareEditValue(AElement: IwbElement; AMaster: IwbElement): boolean;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  if SameText(GetEditValue(AElement), GetEditValue(AMaster)) then
    Exit;

  AddLogEntry('GetEditValue', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function CompareFlags(AElement: IwbElement; AMaster: IwbElement; APath: string; AFlagName: string; ASuggest: boolean; ANotOperator: boolean): boolean;
var
  x         : IwbElement;
  y         : IwbElement;
  a         : IwbElement;
  b         : IwbElement;
  sa        : string;
  sb        : string;
  sTestName : string;
  bResult   : boolean;
begin
  Result := False;

  if TagExists(g_Tag) then
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

  if ANotOperator then
    Result := not SameText(sa, sb)  // only used for Behave Like Exterior, Use Sky Lighting, and Has Water
  else
    Result := StrToBool(sa) or StrToBool(sb);

  if ASuggest and Result then
  begin
    sTestName := IfThen(ANotOperator, 'CompareFlags:NOT', 'CompareFlags:OR');
    AddLogEntry(sTestName, x, y);
    slSuggestedTags.Add(g_Tag);
  end;
end;


function CompareKeys(AElement: IwbElement; AMaster: IwbElement): boolean;
var
  sElementEditValues : string;
  sMasterEditValues  : string;
  ConflictState      : TConflictThis;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  ConflictState := ConflictAllForMainRecord(ContainingMainRecord(AElement));

  if (ConflictState = caUnknown)
  or (ConflictState = caOnlyOne)
  or (ConflictState = caNoConflict) then
    Exit;

  sElementEditValues := EditValues(AElement);
  sMasterEditValues  := EditValues(AMaster);

  if IsEmptyKey(sElementEditValues) and IsEmptyKey(sMasterEditValues) then
    Exit;

  if SameText(sElementEditValues, sMasterEditValues) then
    Exit;

  AddLogEntry('CompareKeys', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function CompareNativeValues(AElement: IwbElement; AMaster: IwbElement; APath: string): boolean;
var
  x : IwbElement;
  y : IwbElement;
begin
  Result := False;

  if TagExists(g_Tag) then
    Exit;

  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  if GetNativeValue(x) = GetNativeValue(y) then
    Exit;

  AddLogEntry('CompareNativeValues', AElement, AMaster);
  slSuggestedTags.Add(g_Tag);

  Result := True;
end;


function SortedArrayElementByValue(AElement: IwbElement; APath: string; AValue: string): IwbElement;
var
  i      : integer;
  kEntry : IwbElement;
begin
  Result := nil;
  for i := 0 to Pred(ElementCount(AElement)) do
  begin
    kEntry := ElementByIndex(AElement, i);
    if SameText(GetElementEditValues(kEntry, APath), AValue) then
    begin
      Result := kEntry;
      Exit;
    end;
  end;
end;


// TODO: natively implemented in 4.1.4
procedure StringListDifference(ASet: TStringList; AOtherSet: TStringList; AOutput: TStringList);
var
  i : integer;
begin
  for i := 0 to Pred(ASet.Count) do
    if AOtherSet.IndexOf(ASet[i]) = -1 then
      AOutput.Add(ASet[i]);
end;


// TODO: natively implemented in 4.1.4
procedure StringListIntersection(ASet: TStringList; AOtherSet: TStringList; AOutput: TStringList);
var
  i : integer;
begin
  for i := 0 to Pred(ASet.Count) do
    if AOtherSet.IndexOf(ASet[i]) > -1 then
      AOutput.Add(ASet[i]);
end;


// TODO: speed this up!
function IsEmptyKey(AEditValues: string): boolean;
var
  i : integer;
begin
  Result := True;
  for i := 1 to Length(AEditValues) do
    if AEditValues[i] = '1' then
    begin
      Result := False;
      Exit;
    end;
end;


function FormatTags(ATags: TStringList; ASingular: string; APlural: string; ANull: string): string;
begin
  if ATags.Count = 1 then
    Result := IntToStr(ATags.Count) + ' ' + ASingular + #13#10#32#32#32#32#32#32
  else
  if ATags.Count > 1 then
    Result := IntToStr(ATags.Count) + ' ' + APlural + #13#10#32#32#32#32#32#32;

  if ATags.Count > 0 then
    Result := Result + Format(' {{BASH:%s}}', [ATags.DelimitedText])
  else
    Result := ANull;
end;


function TagExists(ATag: string): boolean;
begin
  Result := (slSuggestedTags.IndexOf(ATag) <> -1);
end;


procedure Evaluate(AElement: IwbElement; AMaster: IwbElement);
begin
  // exit if the tag already exists
  if TagExists(g_Tag) then
    Exit;

  // Suggest tag if one element exists while the other does not
  if CompareAssignment(AElement, AMaster) then
    Exit;

  // exit if the first element does not exist
  if not Assigned(AElement) then
    Exit;

  // suggest tag if the two elements are different
  if CompareElementCount(AElement, AMaster) then
    Exit;

  // suggest tag if the edit values of the two elements are different
  if CompareEditValue(AElement, AMaster) then
    Exit;

  // compare any number of elements with CompareKeys
  if CompareKeys(AElement, AMaster) then
    Exit;
end;


procedure EvaluateAdd(AElement: IwbElement; AMaster: IwbElement);
begin
  if TagExists(g_Tag) then
    Exit;

  if not Assigned(AElement) then
    Exit;

  // suggest tag if the overriding element has more children than its master
  if CompareElementCountAdd(AElement, AMaster) then
    Exit;
end;


procedure EvaluateChange(AElement: IwbElement; AMaster: IwbElement);
begin
  if TagExists(g_Tag) then
    Exit;

  if not Assigned(AElement) then
    Exit;

  // suggest tag if the two elements and their descendants have different contents
  if CompareKeys(AElement, AMaster) then
    Exit;
end;


procedure EvaluateRemove(AElement: IwbElement; AMaster: IwbElement);
begin
  if TagExists(g_Tag) then
    Exit;

  if not Assigned(AElement) then
    Exit;

  // suggest tag if the master element has more children than its override
  if CompareElementCountRemove(AElement, AMaster) then
    Exit;
end;


procedure EvaluateByPath(AElement: IwbElement; AMaster: IwbElement; APath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  Evaluate(x, y);
end;


procedure EvaluateByPathAdd(AElement: IwbElement; AMaster: IwbElement; APath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  EvaluateAdd(x, y);
end;


procedure EvaluateByPathChange(AElement: IwbElement; AMaster: IwbElement; APath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  EvaluateChange(x, y);
end;


procedure EvaluateByPathRemove(AElement: IwbElement; AMaster: IwbElement; APath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(AElement, APath);
  y := ElementByPath(AMaster, APath);

  EvaluateRemove(x, y);
end;


procedure ProcessTag(ATag: string; e: IInterface; m: IInterface);
var
  x          : IInterface;
  y          : IInterface;
  a          : IInterface;
  b          : IInterface;
  j          : IInterface;
  k          : IInterface;
  sSignature : string;
begin
  g_Tag := ATag;

  if TagExists(g_Tag) then
    Exit;

  sSignature := Signature(e);

  // Bookmark: Actors.ACBS
  if (g_Tag = 'Actors.ACBS') then
  begin
    // assign ACBS elements
    x := ElementBySignature(e, 'ACBS');
    y := ElementBySignature(m, 'ACBS');

    // evaluate Flags if the Use Base Data flag is not set
    a := ElementByName(x, 'Flags');
    b := ElementByName(y, 'Flags');

    if wbIsOblivion and CompareKeys(a, b) then
      Exit;

    if not wbIsOblivion and not CompareFlags(x, y, 'Template Flags', 'Use Base Data', False, False) and CompareKeys(a, b) then
      Exit;

    // evaluate properties
    EvaluateByPath(x, y, 'Fatigue');
    EvaluateByPath(x, y, 'Level');
    EvaluateByPath(x, y, 'Calc min');
    EvaluateByPath(x, y, 'Calc max');
    EvaluateByPath(x, y, 'Speed Multiplier');
    EvaluateByPath(e, m, 'DATA\Base Health');

    // evaluate Barter Gold if the Use AI Data flag is not set
    if wbIsOblivion or not CompareFlags(x, y, 'Template Flags', 'Use AI Data', False, False) then
      EvaluateByPath(x, y, 'Barter gold');
  end

  // Bookmark: Actors.AIData
  else if (g_Tag = 'Actors.AIData') then
  begin
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
    if CompareNativeValues(x, y, 'Buys/Sells and Services') then
      Exit;
  end

  // Bookmark: Actors.AIPackages
  else if (g_Tag = 'Actors.AIPackages') then
    EvaluateByPath(e, m, 'Packages')

  // Bookmark: Actors.Anims
  else if (g_Tag = 'Actors.Anims') then
    EvaluateByPath(e, m, 'KFFZ')

  // Bookmark: Actors.CombatStyle
  else if (g_Tag = 'Actors.CombatStyle') then
    EvaluateByPath(e, m, 'ZNAM')

  // Bookmark: Actors.DeathItem
  else if (g_Tag = 'Actors.DeathItem') then
    EvaluateByPath(e, m, 'INAM')

  // Bookmark: Actors.Perks.Add (TES5, SSE)
  else if (g_Tag = 'Actors.Perks.Add') then
    EvaluateByPathAdd(e, m, 'Perks')

  // Bookmark: Actors.Perks.Change (TES5, SSE)
  else if (g_Tag = 'Actors.Perks.Change') then
    EvaluateByPathChange(e, m, 'Perks')

  // Bookmark: Actors.Perks.Remove (TES5, SSE)
  else if (g_Tag = 'Actors.Perks.Remove') then
    EvaluateByPathRemove(e, m, 'Perks')

  // Bookmark: Actors.RecordFlags (!FO4)
  else if (g_Tag = 'Actors.RecordFlags') then
    EvaluateByPath(e, m, 'Record Header\Record Flags')

  // Bookmark: Actors.Skeleton
  else if (g_Tag = 'Actors.Skeleton') then
  begin
    // assign Model elements
    x := ElementByName(e, 'Model');
    y := ElementByName(m, 'Model');

    // exit if the Model property does not exist in the control record
    if not Assigned(x) then
      Exit;

    // evaluate properties
    EvaluateByPath(x, y, 'MODL');
    EvaluateByPath(x, y, 'MODB');
    EvaluateByPath(x, y, 'MODT');
  end

  // Bookmark: Actors.Spells
  else if (g_Tag = 'Actors.Spells') then
    EvaluateByPath(e, m, 'Spells')

  // Bookmark: Actors.Stats
  else if (g_Tag = 'Actors.Stats') then
  begin
    // assign DATA elements
    x := ElementBySignature(e, 'DATA');
    y := ElementBySignature(m, 'DATA');

    // evaluate CREA properties
    if sSignature = 'CREA' then
    begin
      EvaluateByPath(x, y, 'Health');
      EvaluateByPath(x, y, 'Combat Skill');
      EvaluateByPath(x, y, 'Magic Skill');
      EvaluateByPath(x, y, 'Stealth Skill');
      EvaluateByPath(x, y, 'Attributes');
    end

    // evaluate NPC_ properties
    else if sSignature = 'NPC_' then
    begin
      EvaluateByPath(x, y, 'Base Health');
      EvaluateByPath(x, y, 'Attributes');
      EvaluateByPath(e, m, 'DNAM\Skill Values');
      EvaluateByPath(e, m, 'DNAM\Skill Offsets');
    end;
  end

  // Bookmark: Actors.Voice (FO3, FNV, TES5, SSE)
  else if (g_Tag = 'Actors.Voice') then
    EvaluateByPath(e, m, 'VTCK')

  // Bookmark: C.Acoustic
  else if (g_Tag = 'C.Acoustic') then
    EvaluateByPath(e, m, 'XCAS')

  // Bookmark: C.Climate
  else if (g_Tag = 'C.Climate') then
  begin
    // add tag if the Behave Like Exterior flag is set ine one record but not the other
    if CompareFlags(e, m, 'DATA', 'Behave Like Exterior', True, True) then
      Exit;

    // evaluate additional property
    EvaluateByPath(e, m, 'XCCM');
  end

  // Bookmark: C.Encounter
  else if (g_Tag = 'C.Encounter') then
    EvaluateByPath(e, m, 'XEZN')

  // Bookmark: C.ForceHideLand (!TES4, !FO4)
  else if (g_Tag = 'C.ForceHideLand') then
    EvaluateByPath(e, m, 'XCLC\Land Flags')

  // Bookmark: C.ImageSpace
  else if (g_Tag = 'C.ImageSpace') then
    EvaluateByPath(e, m, 'XCIM')

  // Bookmark: C.Light
  else if (g_Tag = 'C.Light') then
    EvaluateByPath(e, m, 'XCLL')

  // Bookmark: C.Location
  else if (g_Tag = 'C.Location') then
    EvaluateByPath(e, m, 'XLCN')

  // Bookmark: C.LockList
  else if (g_Tag = 'C.LockList') then
    EvaluateByPath(e, m, 'XILL')

  // Bookmark: C.MiscFlags (!FO4)
  else if (g_Tag = 'C.MiscFlags') then
  begin
    if CompareFlags(e, m, 'DATA', 'Is Interior Cell', True, True) then
      Exit;

    if CompareFlags(e, m, 'DATA', 'Can Travel From Here', True, True) then
      Exit;

    if not wbIsOblivion and not wbIsFallout4 then
      if CompareFlags(e, m, 'DATA', 'No LOD Water', True, True) then
        Exit;

    if wbIsOblivion then
      if CompareFlags(e, m, 'DATA', 'Force hide land (exterior cell) / Oblivion interior (interior cell)', True, True) then
        Exit;

    if CompareFlags(e, m, 'DATA', 'Hand Changed', True, True) then
      Exit;
  end

  // Bookmark: C.Music
  else if (g_Tag = 'C.Music') then
    EvaluateByPath(e, m, 'XCMO')

  // Bookmark: FULL (C.Name, Names)
  else if ContainsStr('C.Name Names', g_Tag) then

    EvaluateByPath(e, m, 'FULL')

  // Bookmark: C.Owner
  else if (g_Tag = 'C.Owner') then
    EvaluateByPath(e, m, 'Ownership')

  // Bookmark: C.RecordFlags
  else if (g_Tag = 'C.RecordFlags') then
    EvaluateByPath(e, m, 'Record Header\Record Flags')

  // Bookmark: C.Regions
  else if (g_Tag = 'C.Regions') then
    EvaluateByPath(e, m, 'XCLR')

  // Bookmark: C.SkyLighting
  // add tag if the Behave Like Exterior flag is set in one record but not the other
  else if (g_Tag = 'C.SkyLighting') and CompareFlags(e, m, 'DATA', 'Use Sky Lighting', True, True) then
    Exit

  // Bookmark: C.Water
  else if (g_Tag = 'C.Water') then
  begin
    // add tag if Has Water flag is set in one record but not the other
    if CompareFlags(e, m, 'DATA', 'Has Water', True, True) then
      Exit;

    // exit if Is Interior Cell is set in either record
    if CompareFlags(e, m, 'DATA', 'Is Interior Cell', False, False) then
      Exit;

    // evaluate properties
    EvaluateByPath(e, m, 'XCLW');
    EvaluateByPath(e, m, 'XCWT');
  end

  // Bookmark: Creatures.Blood
  else if (g_Tag = 'Creatures.Blood') then
  begin
    EvaluateByPath(e, m, 'NAM0');
    EvaluateByPath(e, m, 'NAM1');
  end

  // Bookmark: Creatures.Type
  else if (g_Tag = 'Creatures.Type') then
    EvaluateByPath(e, m, 'DATA\Type')

  // Bookmark: Deflst
  else if (g_Tag = 'Deflst') then
    EvaluateByPathRemove(e, m, 'FormIDs')

  // Bookmark: Destructible
  else if (g_Tag = 'Destructible') then
  begin
    // assign Destructable elements
    x := ElementByName(e, 'Destructible');
    y := ElementByName(m, 'Destructible');

    if CompareAssignment(x, y) then
      Exit;

    a := ElementBySignature(x, 'DEST');
    b := ElementBySignature(y, 'DEST');

    // evaluate Destructable properties
    EvaluateByPath(a, b, 'Health');
    EvaluateByPath(a, b, 'Count');
    EvaluateByPath(x, y, 'Stages');

    // assign Destructable flags
    if not wbIsSkyrim then
    begin
      j := ElementByName(a, 'Flags');
      k := ElementByName(b, 'Flags');

      if Assigned(j) or Assigned(k) then
      begin
        // add tag if Destructable flags exist in one record
        if CompareAssignment(j, k) then
          Exit;

        // evaluate Destructable flags
        if CompareKeys(j, k) then
          Exit;
      end;
    end;
  end

  // Bookmark: EffectStats
  else if (g_Tag = 'EffectStats') then
  begin
    if wbIsOblivion or wbIsFallout3 or wbIsFalloutNV then
    begin
      EvaluateByPath(e, m, 'DATA\Flags');

      if not wbIsFallout3 and not wbIsFalloutNV then
        EvaluateByPath(e, m, 'DATA\Base cost');

      if not wbIsOblivion then
        EvaluateByPath(e, m, 'DATA\Associated Item');

      if not wbIsFallout3 and not wbIsFalloutNV then
        EvaluateByPath(e, m, 'DATA\Magic School');

      EvaluateByPath(e, m, 'DATA\Resist Value');
      EvaluateByPath(e, m, 'DATA\Projectile Speed');

      if not wbIsFallout3 and not wbIsFalloutNV then
      begin
        EvaluateByPath(e, m, 'DATA\Constant Effect enchantment factor');
        EvaluateByPath(e, m, 'DATA\Constant Effect barter factor');
      end;

      if wbIsOblivion and CompareFlags(e, m, 'DATA\Flags', 'Use actor value', False, False) then
        EvaluateByPath(e, m, 'DATA\Assoc. Actor Value')
      else if wbIsFallout3 or wbIsFalloutNV then
      begin
        EvaluateByPath(e, m, 'DATA\Archtype');
        EvaluateByPath(e, m, 'DATA\Actor Value');
      end;
    end
    else if wbIsSkyrim then
    begin
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
    end;
  end

  // Bookmark: EnchantmentStats
  else if (g_Tag = 'EnchantmentStats') then
  begin
    if wbIsOblivion or wbIsFallout3 or wbIsFalloutNV then
    begin
      EvaluateByPath(e, m, 'ENIT\Type');
      EvaluateByPath(e, m, 'ENIT\Charge Amount');
      EvaluateByPath(e, m, 'ENIT\Enchant Cost');
      EvaluateByPath(e, m, 'ENIT\Flags');
    end
    else if wbIsSkyrim then
      EvaluateByPath(e, m, 'ENIT');
  end

  // Bookmark: Factions
  else if (g_Tag = 'Factions') then
  begin
    // assign Factions properties
    x := ElementByName(e, 'Factions');
    y := ElementByName(m, 'Factions');

    // add tag if the Factions properties differ
    if CompareAssignment(x, y) then
      Exit;

    // exit if the Factions property in the control record does not exist
    if not Assigned(x) then
      Exit;

    // evaluate Factions properties
    if CompareKeys(x, y) then
      Exit;
  end

  // Bookmark: Graphics
  else if (g_Tag = 'Graphics') then
  begin
    // evaluate Icon and Model properties
    if ContainsStr('ALCH AMMO APPA BOOK INGR KEYM LIGH MGEF MISC SGST SLGM TREE WEAP', sSignature) then
    begin
      EvaluateByPath(e, m, 'Icon');
      EvaluateByPath(e, m, 'Model');
    end

    // evaluate Icon properties
    else if ContainsStr('BSGN CLAS LSCR LTEX REGN', sSignature) then
      EvaluateByPath(e, m, 'Icon')

    // evaluate Model properties
    else if ContainsStr('ACTI DOOR FLOR FURN GRAS STAT', sSignature) then
      EvaluateByPath(e, m, 'Model')

    // evaluate ARMO properties
    else if sSignature = 'ARMO' then
    begin
      // Shared
      EvaluateByPath(e, m, 'Male world model');
      EvaluateByPath(e, m, 'Female world model');

      // ARMO - Oblivion
      if wbIsOblivion then
      begin
        // evaluate Icon properties
        EvaluateByPath(e, m, 'Icon');
        EvaluateByPath(e, m, 'Icon 2 (female)');

        // assign First Person Flags elements
        x := ElementByPath(e, 'BODT\First Person Flags');
        if not Assigned(x) then
          Exit;

        y := ElementByPath(m, 'BODT\First Person Flags');

        // evaluate First Person Flags
        if CompareKeys(x, y) then
          Exit;

        // assign General Flags elements
        x := ElementByPath(e, 'BODT\General Flags');
        if not Assigned(x) then
          Exit;

        y := ElementByPath(m, 'BODT\General Flags');

        // evaluate General Flags
        if CompareKeys(x, y) then
          Exit;
      end

      // ARMO - FO3, FNV
      else if wbIsFallout3 or wbIsFalloutNV then
      begin
        // evaluate Icon properties
        EvaluateByPath(e, m, 'ICON');
        EvaluateByPath(e, m, 'ICO2');

        // assign First Person Flags elements
        x := ElementByPath(e, 'BMDT\Biped Flags');
        if not Assigned(x) then
          Exit;

        y := ElementByPath(m, 'BMDT\Biped Flags');

        // evaluate First Person Flags
        if CompareKeys(x, y) then
          Exit;

        // assign General Flags elements
        x := ElementByPath(e, 'BMDT\General Flags');
        if not Assigned(x) then
          Exit;

        y := ElementByPath(m, 'BMDT\General Flags');

        // evaluate General Flags
        if CompareKeys(x, y) then
          Exit;
      end

      // ARMO - TES5
      else if wbIsSkyrim then
      begin
        // evaluate Icon properties
        EvaluateByPath(e, m, 'Icon');
        EvaluateByPath(e, m, 'Icon 2 (female)');

        // evaluate Biped Model properties
        EvaluateByPath(e, m, 'Male world model');
        EvaluateByPath(e, m, 'Female world model');

        // assign First Person Flags elements
        x  := ElementByPath(e, 'BOD2\First Person Flags');
        if not Assigned(x) then
          Exit;

        y := ElementByPath(m, 'BOD2\First Person Flags');

        // evaluate First Person Flags
        if CompareKeys(x, y) then
          Exit;

        // assign General Flags elements
        x := ElementByPath(e, 'BOD2\General Flags');
        if not Assigned(x) then
          Exit;

        y := ElementByPath(m, 'BOD2\General Flags');

        // evaluate General Flags
        if CompareKeys(x, y) then
          Exit;
      end;
    end

    // evaluate CREA properties
    else if sSignature = 'CREA' then
    begin
      EvaluateByPath(e, m, 'NIFZ');
      EvaluateByPath(e, m, 'NIFT');
    end

    // evaluate EFSH properties
    else if sSignature = 'EFSH' then
    begin
      // evaluate Record Flags
      x := ElementByPath(e, 'Record Header\Record Flags');
      y := ElementByPath(m, 'Record Header\Record Flags');

      if CompareKeys(x, y) then
        Exit;

      // evaluate Icon properties
      EvaluateByPath(e, m, 'ICON');
      EvaluateByPath(e, m, 'ICO2');

      // evaluate other properties
      EvaluateByPath(e, m, 'NAM7');

      if wbIsSkyrim then
      begin
        EvaluateByPath(e, m, 'NAM8');
        EvaluateByPath(e, m, 'NAM9');
      end;

      EvaluateByPath(e, m, 'DATA');
    end

    // evaluate MGEF properties
    else if wbIsSkyrim and (sSignature = 'MGEF') then
    begin
      EvaluateByPath(e, m, 'Magic Effect Data\DATA\Casting Light');
      EvaluateByPath(e, m, 'Magic Effect Data\DATA\Hit Shader');
      EvaluateByPath(e, m, 'Magic Effect Data\DATA\Enchant Shader');
    end

    // evaluate Material property
    else if sSignature = 'STAT' then
      EvaluateByPath(e, m, 'DNAM\Material');
  end

  // Bookmark: Invent.Add
  else if (g_Tag = 'Invent.Add') then
    EvaluateByPathAdd(e, m, 'Items')

  // Bookmark: Invent.Change - TEST
  else if (g_Tag = 'Invent.Change') then
    EvaluateByPathChange(e, m, 'Items')

  // Bookmark: Invent.Remove
  else if (g_Tag = 'Invent.Remove') then
    EvaluateByPathRemove(e, m, 'Items')

  // Bookmark: Keywords
  else if (g_Tag = 'Keywords') then
  begin
    x := ElementBySignature(e, 'KWDA');
    y := ElementBySignature(m, 'KWDA');

    if CompareAssignment(x, y) then
      Exit;

    if CompareElementCount(x, y) then
      Exit;

    x := ElementBySignature(e, 'KSIZ');
    y := ElementBySignature(m, 'KSIZ');

    if CompareAssignment(x, y) then
      Exit;

    if CompareEditValue(x, y) then
      Exit;
  end

  // Bookmark: NPC.AIPackageOverrides
  else if (g_Tag = 'NPC.AIPackageOverrides') then
  begin
    if wbIsSkyrim then
    begin
      EvaluateByPath(e, m, 'SPOR');
      EvaluateByPath(e, m, 'OCOR');
      EvaluateByPath(e, m, 'GWOR');
      EvaluateByPath(e, m, 'ECOR');
    end;
  end

  // Bookmark: NPC.AttackRace
  else if (g_Tag = 'NPC.AttackRace') then
    EvaluateByPath(e, m, 'ATKR')

  // Bookmark: NPC.Class
  else if (g_Tag = 'NPC.Class') then
    EvaluateByPath(e, m, 'CNAM')

  // Bookmark: NPC.CrimeFaction
  else if (g_Tag = 'NPC.CrimeFaction') then
    EvaluateByPath(e, m, 'CRIF')

  // Bookmark: NPC.DefaultOutfit
  else if (g_Tag = 'NPC.DefaultOutfit') then
    EvaluateByPath(e, m, 'DOFT')

  // Bookmark: NPC.Eyes
  else if (g_Tag = 'NPC.Eyes') then
    EvaluateByPath(e, m, 'ENAM')

  // Bookmark: NPC.FaceGen
  else if (g_Tag = 'NPC.FaceGen') then
    EvaluateByPath(e, m, 'FaceGen Data')

  // Bookmark: NPC.Hair
  else if (g_Tag = 'NPC.Hair') then
    EvaluateByPath(e, m, 'HNAM')

  // Bookmark: NPC.Race
  else if (g_Tag = 'NPC.Race') then
    EvaluateByPath(e, m, 'RNAM')

  // Bookmark: ObjectBounds
  else if (g_Tag = 'ObjectBounds') then
    EvaluateByPath(e, m, 'OBND')

  // Bookmark: Outfits.Add
  else if (g_Tag = 'Outfits.Add') then
    EvaluateByPathAdd(e, m, 'OTFT')

  // Bookmark: Outfits.Remove
  else if (g_Tag = 'Outfits.Remove') then
    EvaluateByPathRemove(e, m, 'OTFT')

  // Bookmark: R.AddSpells - DEFER: R.ChangeSpells

  // Bookmark: R.Attributes-F
  else if (g_Tag = 'R.Attributes-F') then
    EvaluateByPath(e, m, 'ATTR\Female')

  // Bookmark: R.Attributes-M
  else if (g_Tag = 'R.Attributes-M') then
    EvaluateByPath(e, m, 'ATTR\Male')

  // Bookmark: R.Body-F
  else if (g_Tag = 'R.Body-F') then
    EvaluateByPath(e, m, 'Body Data\Female Body Data\Parts')

  // Bookmark: R.Body-M
  else if (g_Tag = 'R.Body-M') then
    EvaluateByPath(e, m, 'Body Data\Male Body Data\Parts')

  // Bookmark: R.Body-Size-F
  else if (g_Tag = 'R.Body-Size-F') then
  begin
    EvaluateByPath(e, m, 'DATA\Female Height');
    EvaluateByPath(e, m, 'DATA\Female Weight');
  end

  // Bookmark: R.Body-Size-M
  else if (g_Tag = 'R.Body-Size-M') then
  begin
    EvaluateByPath(e, m, 'DATA\Male Height');
    EvaluateByPath(e, m, 'DATA\Male Weight');
  end

  // Bookmark: R.ChangeSpells
  else if (g_Tag = 'R.ChangeSpells') then
    EvaluateByPath(e, m, 'Spells')

  // Bookmark: R.Description
  else if (g_Tag = 'R.Description') then
    EvaluateByPath(e, m, 'DESC')

  // Bookmark: R.Ears
  else if (g_Tag = 'R.Ears') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[1]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[1]');
  end

  // Bookmark: R.Eyes
  else if (g_Tag = 'R.Eyes') then
    EvaluateByPath(e, m, 'ENAM')

  // Bookmark: R.Hair
  else if (g_Tag = 'R.Hair') then
    EvaluateByPath(e, m, 'HNAM')

  // Bookmark: R.Head
  else if (g_Tag = 'R.Head') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[0]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[0]');
    EvaluateByPath(e, m, 'FaceGen Data');
  end

  // Bookmark: R.Mouth
  else if (g_Tag = 'R.Mouth') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[2]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[2]');
  end

  // Bookmark: R.Relations.Add
  else if (g_Tag = 'R.Relations.Add') then
    EvaluateByPathAdd(e, m, 'Relations')

  // Bookmark: R.Relations.Change - TEST
  else if (g_Tag = 'R.Relations.Change') then
    EvaluateByPathChange(e, m, 'Relations')

  // Bookmark: R.Relations.Remove
  else if (g_Tag = 'R.Relations.Remove') then
    EvaluateByPathRemove(e, m, 'Relations')

  // Bookmark: R.Skills
  else if (g_Tag = 'R.Skills') then
    EvaluateByPath(e, m, 'DATA\Skill Boosts')

  // Bookmark: R.Teeth
  else if (g_Tag = 'R.Teeth') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[3]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[3]');

    // FO3
    if wbIsFallout3 then
    begin
      EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[4]');
      EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[4]');
    end;
  end

  // Bookmark: R.Voice-F
  else if (g_Tag = 'R.Voice-F') then
    EvaluateByPath(e, m, 'VTCK\Voice #1 (Female)')

  // Bookmark: R.Voice-M
  else if (g_Tag = 'R.Voice-M') then
    EvaluateByPath(e, m, 'VTCK\Voice #0 (Male)')

  // Bookmark: Relations.Add
  else if (g_Tag = 'Relations.Add') then
    EvaluateByPathAdd(e, m, 'Relations')

  // Bookmark: Relations.Change - TEST
  else if (g_Tag = 'Relations.Change') then
    EvaluateByPathChange(e, m, 'Relations')

  // Bookmark: Relations.Remove
  else if (g_Tag = 'Relations.Remove') then
    EvaluateByPathRemove(e, m, 'Relations')

  // Bookmark: Roads
  else if (g_Tag = 'Roads') then
    EvaluateByPath(e, m, 'PGRP')

  // Bookmark: Scripts
  else if (g_Tag = 'Scripts') then
    EvaluateByPath(e, m, 'SCRI')

  // Bookmark: Sound
  else if (g_Tag = 'Sound') then
  begin
    // Activators, Containers, Doors, and Lights
    if ContainsStr('ACTI CONT DOOR LIGH', sSignature) then
    begin
      EvaluateByPath(e, m, 'SNAM');

      // Activators
      if sSignature = 'ACTI' then
        EvaluateByPath(e, m, 'VNAM')

      // Containers
      else if sSignature = 'CONT' then
      begin
        EvaluateByPath(e, m, 'QNAM');
        if not wbIsSkyrim and not wbIsFallout3 then
          EvaluateByPath(e, m, 'RNAM');  // FO3, TESV, and SSE don't have this element
      end

      // Doors
      else if sSignature = 'DOOR' then
      begin
        EvaluateByPath(e, m, 'ANAM');
        EvaluateByPath(e, m, 'BNAM');
      end;
    end

    // Creatures
    else if sSignature = 'CREA' then
    begin
      EvaluateByPath(e, m, 'WNAM');
      EvaluateByPath(e, m, 'CSCR');
      EvaluateByPath(e, m, 'Sound Types');
    end

    // Magic Effects
    else if sSignature = 'MGEF' then
    begin
      // TES5, SSE
      if wbIsSkyrim then
        EvaluateByPath(e, m, 'SNDD')

      // FO3, FNV, TES4
      else
      begin
        EvaluateByPath(e, m, 'DATA\Effect sound');
        EvaluateByPath(e, m, 'DATA\Bolt sound');
        EvaluateByPath(e, m, 'DATA\Hit sound');
        EvaluateByPath(e, m, 'DATA\Area sound');
      end;
    end

    // Weather
    else if sSignature = 'WTHR' then
      EvaluateByPath(e, m, 'Sounds');
  end

  // Bookmark: SpellStats
  else if (g_Tag = 'SpellStats') then
    EvaluateByPath(e, m, 'SPIT')

  // Bookmark: Stats
  else if (g_Tag = 'Stats') then
  begin
    if ContainsStr('ALCH AMMO APPA ARMO BOOK CLOT INGR KEYM LIGH MISC SGST SLGM WEAP', sSignature) then
    begin
      EvaluateByPath(e, m, 'EDID');
      EvaluateByPath(e, m, 'DATA');

      if ContainsStr('ARMO WEAP', sSignature) then
        EvaluateByPath(e, m, 'DNAM')

      else if sSignature = 'WEAP' then
        EvaluateByPath(e, m, 'CRDT');
    end

    else if sSignature = 'ARMA' then
      EvaluateByPath(e, m, 'DNAM');
  end

  // Bookmark: Text
  else if (g_Tag = 'Text') then
  begin
    if ContainsStr('ALCH AMMO APPA ARMO AVIF BOOK BSGN CHAL CLAS IMOD LSCR MESG MGEF PERK SCRL SHOU SKIL SPEL TERM WEAP', sSignature) then
      EvaluateByPath(e, m, 'DESC')

    else if not wbIsOblivion then begin
      if sSignature = 'BOOK' then
        EvaluateByPath(e, m, 'CNAM')

      else if sSignature = 'MGEF' then
        EvaluateByPath(e, m, 'DNAM')

      else if sSignature = 'NOTE' then
        EvaluateByPath(e, m, 'TNAM');
    end;
  end

  // Bookmark: WeaponMods
  else if (g_Tag = 'WeaponMods') then
    EvaluateByPath(e, m, 'Weapon Mods');
end;


procedure ProcessDelevRelevTags(ARecord: IwbMainRecord; AMaster: IwbMainRecord);
var
  kEntries          : IwbElement;
  kEntriesMaster    : IwbElement;
  kEntry            : IwbElement;
  kEntryMaster      : IwbElement;
  kCOED             : IwbElement; // extra data
  kCOEDMaster       : IwbElement; // extra data
  sSignature        : string;
  sEditValues       : string;
  sMasterEditValues : string;
  i                 : integer;
  j                 : integer;
begin
  // nothing to do if already tagged
  if TagExists('Delev') and TagExists('Relev') then
    Exit;

  // get Leveled List Entries
  kEntries       := ElementByName(ARecord, 'Leveled List Entries');
  kEntriesMaster := ElementByName(AMaster, 'Leveled List Entries');

  if not Assigned(kEntries) then
    Exit;

  if not Assigned(kEntriesMaster) then
    Exit;

  // initalize count matched on reference entries
  j := 0;

  if not TagExists('Relev') then
  begin
    g_Tag := 'Relev';

    for i := 0 to Pred(ElementCount(kEntries)) do
    begin
      kEntry := ElementByIndex(kEntries, i);
      kEntryMaster := SortedArrayElementByValue(kEntriesMaster, 'LVLO\Reference', GetElementEditValues(kEntry, 'LVLO\Reference'));

      if not Assigned(kEntryMaster) then
        Continue;

      Inc(j);

      if TagExists(g_Tag) then
        Continue;

      if CompareNativeValues(kEntry, kEntryMaster, 'LVLO\Level') then
        Exit;

      if CompareNativeValues(kEntry, kEntryMaster, 'LVLO\Count') then
        Exit;

      if wbIsOblivion then
        Continue;

      // Relev check for changed level, count, extra data
      kCOED       := ElementBySignature(kEntry, 'COED');
      kCOEDMaster := ElementBySignature(kEntryMaster, 'COED');

      sEditValues       := EditValues(kCOED);
      sMasterEditValues := EditValues(kCOEDMaster);

      if not SameText(sEditValues, sMasterEditValues) then
      begin
        AddLogEntry('Assigned', kCOED, kCOEDMaster);
        slSuggestedTags.Add(g_Tag);
        Exit;
      end;
    end;
  end;

  if not TagExists('Delev') then
  begin
    g_Tag := 'Delev';

    sSignature := Signature(ARecord);

    if (((sSignature = 'LVLC') and (wbIsOblivion or wbIsFallout3 or wbIsFalloutNV))
    or (sSignature = 'LVLI') or ((sSignature = 'LVLN') and not wbIsOblivion)
    or ((sSignature = 'LVSP') and (wbIsOblivion or wbIsSkyrim)))
    and not TagExists(g_Tag) then
      // if number of matched entries less than in master list
      if j < ElementCount(kEntriesMaster) then
      begin
        AddLogEntry('ElementCount', kEntries, kEntriesMaster);
        slSuggestedTags.Add(g_Tag);
        Exit;
      end;
  end;
end;


function AddLogEntry(ATestName: string; AElement: IwbElement; AMaster: IwbElement): string;
var
  mr    : IwbMainRecord;
  sName : string;
  sPath : string;
begin
  if not g_LogTests then
    Exit;

  if Assigned(AMaster) then
  begin
    mr    := ContainingMainRecord(AMaster);
    sPath := Path(AMaster);
  end else
  begin
    mr    := ContainingMainRecord(AElement);
    sPath := Path(AElement);
  end;

  sPath := RightStr(sPath, Length(sPath) - 5);

  sName := Format('[%s:%s]', [Signature(mr), IntToHex(GetLoadOrderFormID(mr), 8)]);

  slLog.Add(Format('{%s} (%s) %s %s', [g_Tag, ATestName, sName, sPath]));
end;


function FileByName(AFileName: string): IwbFile;
var
  kFile : IwbFile;
  i     : integer;
begin
  Result := nil;

  for i := 0 to Pred(FileCount) do
  begin
    kFile := FileByIndex(i);
    if SameText(AFileName, GetFileName(kFile)) then
    begin
      Result := kFile;
      Exit;
    end;
  end;
end;


procedure EscKeyHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Sender.Close;
end;


procedure chkAddTagsClick(Sender: TObject);
begin
  g_AddTags := Sender.Checked;
end;


procedure chkLoggingClick(Sender: TObject);
begin
  g_LogTests := Sender.Checked;
end;


function ShowPrompt(ACaption: string): integer;
var
  frm        : TForm;
  chkAddTags : TCheckBox;
  chkLogging : TCheckBox;
  btnCancel  : TButton;
  btnOk      : TButton;
  i          : integer;
begin
  Result := mrCancel;

  frm := TForm.Create(TForm(frmMain));

  try
    frm.Caption      := ACaption;
    frm.BorderStyle  := bsToolWindow;
    frm.ClientWidth  := 234 * ScaleFactor;
    frm.ClientHeight := 134 * ScaleFactor;
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
    chkAddTags.Checked  := False;
    g_AddTags := chkAddTags.Checked;
    chkAddTags.OnClick  := chkAddTagsClick;
    chkAddTags.TabOrder := 0;

    chkLogging := TCheckBox.Create(frm);
    chkLogging.Parent   := frm;
    chkLogging.Left     := 16 * ScaleFactor;
    chkLogging.Top      := 39 * ScaleFactor;
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
    btnOk.Top                 := 100 * ScaleFactor;
    btnOk.Width               := 75 * ScaleFactor;
    btnOk.Height              := 25 * ScaleFactor;
    btnOk.Caption             := 'Run';
    btnOk.Default             := True;
    btnOk.ModalResult         := mrOk;
    btnOk.TabOrder            := 3;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent          := frm;
    btnCancel.Left            := 143 * ScaleFactor;
    btnCancel.Top             := 100 * ScaleFactor;
    btnCancel.Width           := 75 * ScaleFactor;
    btnCancel.Height          := 25 * ScaleFactor;
    btnCancel.Caption         := 'Abort';
    btnCancel.ModalResult     := mrAbort;
    btnCancel.TabOrder        := 4;

    Result := frm.ShowModal;
  finally
    frm.Free;
  end;
end;

end.