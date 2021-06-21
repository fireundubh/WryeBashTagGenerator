{
  Purpose: Generates bash tags for a selected plugin automatically
  Games:   FO3/FNV/FO4/TES4/TES5/SSE/Enderal
  Author:  fireundubh <fireundubh@gmail.com>
}

unit WryeBashTagGenerator;


const
  scaleFactor = Screen.PixelsPerInch / 96;


var
  kFile            : IwbFile;
  slBadTags        : TStringList;
  slDifferentTags  : TStringList;
  slExistingTags   : TStringList;
  slLog            : TStringList;
  slSuggestedTags  : TStringList;
  slDeprecatedTags : TStringList;
  g_sTag           : string;
  sFileName        : string;
  sScriptName      : string;
  sScriptVersion   : string;
  sScriptAuthor    : string;
  sScriptEmail     : string;
  optionAddTags    : integer;
  optionOutputLog  : integer;


function wbIsOblivion: boolean;
begin
  Result := wbGameMode = gmTES4;
end;


function wbIsSkyrim: boolean;
begin
  Result := (wbGameMode = gmTES5) or (wbGameMode = gmEnderal) or (wbGameMode = gmTES5VR) or (wbGameMode = gmSSE) or (wbGameMode = gmEnderalSE);
end;


function wbIsSkyrimSE: boolean;
begin
  Result := (wbGameMode = gmSSE) or (wbGameMode = gmEnderalSE);
end;


function wbIsFallout3: boolean;
begin
  Result := wbGameMode = gmFO3;
end;


function wbIsFalloutNV: boolean;
begin
  Result := wbGameMode = gmFNV;
end;


function wbIsFallout4: boolean;
begin
  Result := (wbGameMode = gmFO4) or (wbGameMode = gmFO4VR);
end;


function wbIsFallout76: boolean;
begin
  Result := wbGameMode = gmFO76;
end;


function wbIsEnderal: boolean;
begin
  Result := wbGameMode = gmEnderal;
end;


function wbIsEnderalSE: boolean;
begin
  Result := wbGameMode = gmEnderalSE;
end;


procedure LogInfo(s: String);
begin
  AddMessage('[INFO] ' + s);
end;


procedure LogWarn(s: String);
begin
  AddMessage('[WARN] ' + s);
end;


procedure LogError(s: String);
begin
  AddMessage('[ERRO] ' + s);
end;


function Initialize: integer;
var
  kDescription : IInterface;
  kHeader      : IInterface;
  sBashTags    : string;
  sDescription : string;
  sMasterName  : string;
  r            : IwbMainRecord;
  i            : integer;
begin
  sScriptName    := 'WryeBashTagGenerator';
  sScriptVersion := '1.6.4.0';
  sScriptAuthor  := 'fireundubh';
  sScriptEmail   := 'fireundubh@gmail.com';

  // clear
  ClearMessages();

  // show script header
  AddMessage(#10);
  LogInfo(sScriptName + ' v' + sScriptVersion + ' by ' + sScriptAuthor + ' <' + sScriptEmail + '>');
  AddMessage(#10);

  optionAddTags   := mrNo;
  optionOutputLog := mrYes;

  kFile := Configure(sScriptName + ' v' + sScriptVersion);
  if not Assigned(kFile) then
    Exit;

  sFileName := GetFileName(kFile);

  // create list of log entries
  slLog := TStringList.Create;
  slLog.Sorted := False;
  slLog.Duplicates := dupAccept;

  // create list of tags
  slSuggestedTags  := TStringList.Create;
  slSuggestedTags.Sorted := True;
  slSuggestedTags.Duplicates := dupIgnore;
  slSuggestedTags.Delimiter := ',';  // separated by comma

  slExistingTags   := TStringList.Create;  // existing tags

  slDifferentTags  := TStringList.Create;  // different tags
  slDifferentTags.Sorted := True;
  slDifferentTags.Duplicates := dupIgnore;

  slBadTags        := TStringList.Create;  // bad tags
  slDeprecatedTags := TStringList.Create;  // deprecated tags
  slDeprecatedTags.CommaText := 'Body-F,Body-M,Body-Size-F,Body-Size-M,C.GridFlags,Derel,Eyes,Eyes-D,Eyes-E,Eyes-R,Hair,Invent,InventOnly,Merge,Npc.EyesOnly,Npc.HairOnly,NpcFaces,R.Relations,Relations,ScriptContents';

  if wbIsFallout76 then
  begin
    LogError('Fallout 76 is not supported by CBash.');
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
  else if wbIsSkyrim and not wbIsSkyrimSE then
    LogInfo('Using game mode: Skyrim')
  else if wbIsSkyrimSE then
    LogInfo('Using game mode: Skyrim Special Edition')
  else if wbIsEnderal then
    LogInfo('Using game mode: Enderal')
  else if wbIsEnderalSE then
    LogInfo('Using game mode: Enderal Special Edition')
  else
  begin
    LogError('Cannot identify game mode');
    Exit;
  end;

  AddMessage(#10);

  LogInfo('Processing... Please wait. This could take a while.');

  for i := 0 to Pred(RecordCount(kFile)) do
    ProcessRecord(RecordByIndex(kFile, i));

  AddMessage(#10);

  // exit conditions
  if not Assigned(sFileName) then
    Exit;

  // output file name
  LogInfo(Uppercase(sFileName));

  AddMessage(#10);

  // output log
  if optionOutputLog = mrYes then
    for i := 0 to Pred(slLog.Count) do
      LogInfo(slLog[i]);

  if (optionOutputLog = mrYes) and (slLog.Count > 0) then
    AddMessage(#10);

  // if any suggested tags were generated
  if slSuggestedTags.Count > 0 then
  begin
    kHeader := ElementBySignature(kFile, 'TES4');

    // determine if the header record exists
    if Assigned(kHeader) then
    begin
      kDescription := ElementBySignature(kHeader, 'SNAM');
      sDescription := GetEditValue(kDescription);

      // categorize tag list
      sBashTags := RegExMatch('{{BASH:.*?}}', sDescription);
      if Length(sBashTags) > 0 then
      begin
        sBashTags := Trim(MidStr(sBashTags, 8, Length(sBashTags) - 9));
        slExistingTags.CommaText := sBashTags;
      end else
        slExistingTags.CommaText := '';

      if optionAddTags = mrNo then
      begin
        StringListIntersection(slExistingTags, slDeprecatedTags, slBadTags);
        LogInfo(FormatTags(slBadTags, 'deprecated tag found:', 'deprecated tags found:', 'No deprecated tags found.'));
        slBadTags.Clear;
      end;

      StringListDifference(slSuggestedTags, slExistingTags, slDifferentTags);
      StringListDifference(slExistingTags, slSuggestedTags, slBadTags);
      slSuggestedTags.AddStrings(slDifferentTags);

      // exit if existing and suggested tags are the same
      if SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) then
      begin
        LogInfo(FormatTags(slExistingTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.'));
        LogInfo(FormatTags(slSuggestedTags, 'suggested tag:', 'suggested tags:', 'No suggested tags.'));
        LogWarn('No tags to add.' + #13#10);
        Exit;
      end;

    // exit if the header record doesn't exist
    end else begin
      LogError('Header record not found. Nothing to do. Exiting.' + #13#10);
      Exit;
    end;

    // write tags
    if optionAddTags = mrYes then
    begin
      // if the description element doesn't exist, add the element
      kDescription := ElementBySignature(kHeader, 'SNAM');
      if not Assigned(kDescription) then
        kDescription := Add(kHeader, 'SNAM', True);

      if not SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) then
      begin
        sDescription := RegExReplace('{{BASH:.*?}}', Format('{{BASH:%s}}', [slSuggestedTags.DelimitedText]), GetEditValue(kDescription));
        SetEditValue(kDescription, sDescription);
      end;

      LogInfo(FormatTags(slBadTags,       'bad tag removed:',          'bad tags removed:',          'No bad tags found.'));
      LogInfo(FormatTags(slDifferentTags, 'tag added to file header:', 'tags added to file header:', 'No tags added.'));
    end;

    // suggest tags only and output to log
    if optionAddTags = mrNo then
    begin
      LogInfo(FormatTags(slExistingTags,  'existing tag found:',    'existing tags found:',    'No existing tags found.'));
      LogInfo(FormatTags(slBadTags,       'bad tag found:',         'bad tags found:',         'No bad tags found.'));
      LogInfo(FormatTags(slDifferentTags, 'suggested tag to add:',  'suggested tags to add:',  'No suggested tags to add.'));
      LogInfo(FormatTags(slSuggestedTags, 'suggested tag overall:', 'suggested tags overall:', 'No suggested tags overall.'));
    end;
  end else
    LogInfo('No tags are suggested for this plugin.');

  AddMessage(#10);
end;


function ProcessRecord(e: IInterface): integer;
var
  o             : IInterface;
  sSignature    : string;
  ConflictState : TConflictThis;
  iFormID       : integer;
begin
  // exit conditions
  ConflictState := ConflictAllForMainRecord(e);

  // get record signature
  sSignature := Signature(e);

  if (ConflictState = caUnknown)
  or (ConflictState = caOnlyOne)
  or (ConflictState = caNoConflict) then
    Exit;

  // exit if the record should not be processed
  if CompareText(sFileName, 'Dawnguard.esm') = 0 then
  begin
    iFormID := FileFormID(e);
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
    if ContainsStr(sSignature, 'CREA NPC_') then
    begin
      ProcessTag('Actors.Spells', e, o);

      if sSignature = 'CREA' then
        ProcessTag('Creatures.Blood', e, o);
    end

    // TODO: Npc.EyesOnly - NOT IMPLEMENTED
    // TODO: Npc.HairOnly - NOT IMPLEMENTED
    // TODO: R.AddSpells - NOT IMPLEMENTED

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

    else if ContainsStr(sSignature, 'ACTI ALCH AMMO ARMO BOOK FLOR FURN INGR KEYM LCTN MGEF MISC NPC_ SCRL SLGM SPEL TACT WEAP') then
      ProcessTag('Keywords', e, o)

    // added in Wrye Bash 307 Beta 6
    else if sSignature = 'NPC_' then
    begin
      ProcessTag('Actors.Perks.Add', e, o);
      ProcessTag('Actors.Perks.Change', e, o);
      ProcessTag('Actors.Perks.Remove', e, o);
      ProcessTag('Factions', e, o);
    end

    // added in Wrye Bash 307 Beta 6
    else if sSignature = 'FACT' then
    begin
      ProcessTag('Relations.Add', e, o);
      ProcessTag('Relations.Change', e, o);
      ProcessTag('Relations.Remove', e, o);
    end;
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV
  // -------------------------------------------------------------------------------
  if wbIsFallout3 or wbIsFalloutNV then
  begin
    if sSignature = 'FLST' then
      ProcessTag('Deflst', e, o);

    g_sTag := 'Destructible';
    if ContainsStr(sSignature, 'ACTI ALCH AMMO BOOK CONT DOOR FURN IMOD KEYM MISC MSTT PROJ TACT TERM WEAP') then
      ProcessTag('Destructible', e, o)

    // special handling for CREA and NPC_ record types
    else if ContainsStr(sSignature, 'CREA NPC_') then
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
        ProcessTag('Destructible', e, o);

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
  if wbIsOblivion or wbIsFallout3 or wbIsFalloutNV then
  begin
    if ContainsStr(sSignature, 'CREA NPC_') then
    begin
      if sSignature = 'CREA' then
        ProcessTag('Creatures.Type', e, o);

      g_sTag := 'Factions';
      if wbIsOblivion or not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Factions', False, False) then
        ProcessTag('Factions', e, o);

      if sSignature = 'NPC_' then
      begin
        ProcessTag('NPC.Eyes', e, o);
        ProcessTag('NPC.FaceGen', e, o);
        ProcessTag('NPC.Hair', e, o);
      end;
    end

    else if sSignature = 'FACT' then
    begin
      ProcessTag('Relations.Add', e, o);
      ProcessTag('Relations.Change', e, o);
      ProcessTag('Relations.Remove', e, o);
    end;
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES5, SSE
  // -------------------------------------------------------------------------------
  if not wbIsOblivion and not wbIsFallout4 then
  begin
    if ContainsStr(sSignature, 'CREA NPC_') then
    begin
      g_sTag := 'Actors.ACBS';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
        ProcessTag('Actors.ACBS', e, o);

      g_sTag := 'Actors.AIData';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Data', False, False) then
        ProcessTag('Actors.AIData', e, o);

      g_sTag := 'Actors.AIPackages';
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

      g_sTag := 'Actors.Skeleton';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
        ProcessTag('Actors.Skeleton', e, o);

      g_sTag := 'Actors.Stats';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
        ProcessTag('Actors.Stats', e, o);

      if wbIsFallout3 or wbIsFalloutNV or (sSignature = 'NPC_')
        ProcessTag('Actors.Voice', e, o);

      if sSignature = 'NPC_' then
      begin
        g_sTag := 'NPC.AIPackageOverrides';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) then
          ProcessTag('NPC.AIPackageOverrides', e, o);

        ProcessTag('NPC.AttackRace', e, o);

        g_sTag := 'NPC.Class';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
          ProcessTag('NPC.Class', e, o);

        ProcessTag('NPC.CrimeFaction', e, o);
        ProcessTag('NPC.DefaultOutfit', e, o);

        g_sTag := 'NPC.Race';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
          ProcessTag('NPC.Race', e, o);
      end;

      g_sTag := 'Scripts';
      if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Script', False, False) then
        ProcessTag(g_sTag, e, o);
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
      ProcessTag('R.Eyes', e, o);
      ProcessTag('R.Hair', e, o);
      ProcessTag('R.Ears', e, o);
      ProcessTag('R.Head', e, o);
      ProcessTag('R.Mouth', e, o);
      ProcessTag('R.Teeth', e, o);
      ProcessTag('R.Relations.Add', e, o);
      ProcessTag('R.Relations.Change', e, o);
      ProcessTag('R.Relations.Remove', e, o);
      ProcessTag('R.Body-F', e, o);
      ProcessTag('R.Body-M', e, o);
      ProcessTag('R.Skills', e, o);
      ProcessTag('R.Body-Size-F', e, o);
      ProcessTag('R.Body-Size-M', e, o);
      ProcessTag('R.Description', e, o);
      ProcessTag('Voice-F', e, o);
      ProcessTag('Voice-M', e, o);
    end;

    // TODO: ScriptContents - SHOULD NOT BE IMPLEMENTED
    // -- According to the Wrye Bash Readme: "Should not be used. Can cause serious issues."

    if ContainsStr(sSignature, 'ACTI ALCH ARMO CONT DOOR FLOR FURN INGR KEYM LIGH LVLC MISC QUST WEAP') then
      ProcessTag('Scripts', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES4, TES5, SSE
  // -------------------------------------------------------------------------------

  if not wbIsFallout4 then
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
    if ContainsStr(sSignature, 'LVLC LVLI LVLN LVSP') then
      ProcessDelevRelevTags(e, o);

    if ContainsStr(sSignature, 'ACTI ALCH AMMO APPA ARMO BOOK BSGN CLAS CLOT DOOR FLOR FURN INGR KEYM LIGH MGEF MISC SGST SLGM WEAP') then
    begin
      ProcessTag('Graphics', e, o);
      ProcessTag('Names', e, o);
      ProcessTag('Stats', e, o);

      if ContainsStr(sSignature, 'ACTI DOOR LIGH MGEF') then
      begin
        ProcessTag('Sound', e, o);

        if sSignature = 'MGEF' then
          ProcessTag('EffectStats', e, o);
      end;
    end;

    if ContainsStr(sSignature, 'CREA EFSH GRAS LSCR LTEX REGN STAT TREE') then
      ProcessTag('Graphics', e, o);

    if sSignature = 'CONT' then
    begin
      ProcessTag('Invent.Add', e, o);
      ProcessTag('Invent.Change', e, o);
      ProcessTag('Invent.Remove', e, o);
      ProcessTag('Names', e, o);
      ProcessTag('Sound', e, o);
    end;

    if ContainsStr(sSignature, 'DIAL ENCH EYES FACT HAIR QUST RACE SPEL WRLD') then
    begin
      ProcessTag('Names', e, o);

      if sSignature = 'ENCH' then
        ProcessTag('EnchantmentStats', e, o);
    end;

    if (sSignature = 'WTHR') then
      ProcessTag('Sound', e, o);

    // special handling for CREA and NPC_
    if ContainsStr(sSignature, 'CREA NPC_') then
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
        g_sTag := 'Invent.Add';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(g_sTag, e, o);

        g_sTag := 'Invent.Change';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(g_sTag, e, o);

        g_sTag := 'Invent.Remove';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(g_sTag, e, o);

        // special handling for CREA and NPC_ record types
        g_sTag := 'Names';
        if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Base Data', False, False) then
          ProcessTag(g_sTag, e, o);

        // special handling for CREA record type
        g_sTag := 'Sound';
        if sSignature = 'CREA' then
          if not CompareFlags(e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
            ProcessTag(g_sTag, e, o);
      end;
    end;
  end;

  // ObjectBounds
  g_sTag := 'ObjectBounds';

  if wbIsFallout3 and ContainsStr(sSignature, 'ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK COBJ CONT CREA DOOR EXPL FURN GRAS IDLM INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP') then
    ProcessTag(g_sTag, e, o);

  if wbIsFalloutNV and ContainsStr(sSignature, 'ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK CCRD CHIP CMNY COBJ CONT CREA DOOR EXPL FURN GRAS IDLM IMOD INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP') then
    ProcessTag(g_sTag, e, o);

  if wbIsSkyrim and ContainsStr(sSignature, 'ACTI ADDN ALCH AMMO APPA ARMO ARTO ASPC BOOK CONT DOOR DUAL ENCH EXPL FLOR FURN GRAS HAZD IDLM INGR KEYM LIGH LVLI LVLN LVSP MISC MSTT NPC_ PROJ SCRL SLGM SOUN SPEL STAT TACT TREE TXST WEAP') then
    ProcessTag(g_sTag, e, o);

  if wbIsFallout4 and ContainsStr(sSignature, 'LVLI LVLN') then
    ProcessTag(g_sTag, e, o);

  // Text
  if not wbIsFallout4 then
  begin
    g_sTag := 'Text';

    if wbIsOblivion and ContainsStr(sSignature, 'BOOK BSGN CLAS LSCR MGEF SKIL') then
      ProcessTag(g_sTag, e, o);

    if wbIsFallout3 and ContainsStr(sSignature, 'AVIF BOOK CLAS LSCR MESG MGEF NOTE PERK TERM') then
      ProcessTag(g_sTag, e, o);

    if wbIsFalloutNV and ContainsStr(sSignature, 'AVIF BOOK CHAL CLAS IMOD LSCR MESG MGEF NOTE PERK TERM') then
      ProcessTag(g_sTag, e, o);

    if wbIsSkyrim and ContainsStr(sSignature, 'ALCH AMMO APPA ARMO AVIF BOOK CLAS LSCR MESG MGEF SCRL SHOU SPEL WEAP') then
      ProcessTag(g_sTag, e, o);
  end;
end;

function Finalize: integer;
begin
  if not Assigned(kFile) then
  begin
    LogInfo('Script execution was aborted.' + #13#10);
    Exit;
  end;

  slLog.Free;
  slSuggestedTags.Free;
  slExistingTags.Free;
  slDifferentTags.Free;
  slBadTags.Free;
  slDeprecatedTags.Free;
end;


function StrToBool(s: String): boolean;
begin
  if (s <> '0') and (s <> '1') then
    Result := nil
  else
    if s = '1' then
      Result := True
    else
      Result := False;
end;


function RegExMatch(asPattern: string; asSubject: string): string;
var
  re     : TPerlRegEx;
begin
  Result := asSubject;
  re := TPerlRegEx.Create;
  try
    re.RegEx := asPattern;
    re.Options := [];
    re.Subject := asSubject;
    if re.Match then
      Result := re.MatchedText;
  finally
    re.Free;
  end;
end;


function RegExReplace(const asExpression: string; asReplacement: string; asSubject: string): string;
var
  re     : TPerlRegEx;
  output : string;
begin
  Result := asSubject;
  re := TPerlRegEx.Create;
  try
    re.RegEx := asExpression;
    re.Options := [];
    re.Subject := asSubject;
    re.Replacement := asReplacement;
    re.ReplaceAll;
    output := re.Subject;
  finally
    re.Free;
    Result := output;
  end;
end;


function RemoveFromEnd(asSource: string; asSubstring: string): string;
begin
  Result := asSource;
  if EndsText(asSource, asSubstring) then
    Result := Copy(asSource, 1, Length(asSource) - Length(asSubstring));
end;


function SortKeyEx(const akElement: IInterface): string;
var
  kElement: IInterface;
  i: integer;
begin
  Result := GetEditValue(akElement);

  for i := 0 to Pred(ElementCount(akElement)) do
  begin
    kElement := ElementByIndex(akElement, i);

    if SameText(Name(kElement), 'unknown') or SameText(Name(kElement), 'unused') then
      Continue;

    if Result <> '' then
      Result := Result + ' ' + SortKeyEx(kElement)
    else
      Result := SortKeyEx(kElement);
  end;
end;


function CompareAssignment(e: IInterface; m: IInterface): boolean;
var
  bAssignedE : boolean;
  bAssignedM : boolean;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  bAssignedE := Assigned(e);
  bAssignedM := Assigned(m);

  if (not bAssignedE and not bAssignedM)
  or (bAssignedE and bAssignedM) then
    Exit;

  if bAssignedE <> bAssignedM then
  begin
    AddLogEntry('Assigned', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function CompareElementCount(e: IInterface; m: IInterface): boolean;
var
  iCountE : integer;
  iCountM : integer;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  iCountE := ElementCount(e);
  iCountM := ElementCount(m);

  if iCountE = iCountM then
    Exit;

  if iCountE <> iCountM then
  begin
    AddLogEntry('ElementCount', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function CompareElementCountAdd(e: IInterface; m: IInterface): boolean;
var
  iCountE : integer;
  iCountM : integer;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  iCountE := ElementCount(e);
  iCountM := ElementCount(m);

  if iCountE = iCountM then
    Exit;

  if iCountE < iCountM then
  begin
    AddLogEntry('ElementCountAdd', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function CompareElementCountRemove(e: IInterface; m: IInterface): boolean;
var
  iCountE : integer;
  iCountM : integer;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  iCountE := ElementCount(e);
  iCountM := ElementCount(m);

  if iCountE = iCountM then
    Exit;

  if iCountE > iCountM then
  begin
    AddLogEntry('ElementCountRemove', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function CompareEditValue(e: IInterface; m: IInterface): boolean;
var
  sValueE : string;
  sValueM : string;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  sValueE := GetEditValue(e);
  sValueM := GetEditValue(m);

  if SameText(sValueE, sValueM) then
    Exit;

  if not SameText(sValueE, sValueM) then
  begin
    AddLogEntry('GetEditValue', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function CompareFlags(e: IInterface; m: IInterface; asPath: string; asFlagName: string; abAddTag: boolean; abOperation: boolean): boolean;
var
  x         : IInterface;
  y         : IInterface;
  a         : IInterface;
  b         : IInterface;
  sa        : string;
  sb        : string;
  sTestName : string;
  bResult   : boolean;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  // flags arrays
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  // individual flags
  a := ElementByName(x, asFlagName);
  b := ElementByName(y, asFlagName);

  // individual flag edit values
  sa := GetEditValue(a);
  sb := GetEditValue(b);

  if abOperation then
    bResult := not SameText(sa, sb)  // only used for Behave Like Exterior, Use Sky Lighting, and Has Water
  else
    bResult := StrToBool(sa) or StrToBool(sb);

  if abAddTag and bResult then
  begin
    sTestName := IfThen(abOperation, 'CompareFlags:NOT', 'CompareFlags:OR');
    AddLogEntry(sTestName, x, y);
    slSuggestedTags.Add(g_sTag);
  end;

  Result := bResult;
end;


function CompareKeys(e: IInterface; m: IInterface): boolean;
var
  bResult       : boolean;
  sKeyE         : string;
  sKeyM         : string;
  ConflictState : TConflictThis;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  ConflictState := ConflictAllForMainRecord(ContainingMainRecord(e));

  if (ConflictState = caUnknown)
  or (ConflictState = caOnlyOne)
  or (ConflictState = caNoConflict) then
    Exit;

  sKeyE := SortKeyEx(e);
  sKeyM := SortKeyEx(m);

  // empty check
  if (IsEmptyKey(sKeyE) and IsEmptyKey(sKeyM))
  or SameText(sKeyE, sKeyM) then
    Exit;

  // case sensitive comparison
  if not SameText(sKeyE, sKeyM) then
  begin
    AddLogEntry('CompareKeys', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function CompareNativeValues(e: IInterface; m: IInterface; asPath: string): boolean;
var
  x : IInterface;
  y : IInterface;
begin
  if TagExists(g_sTag) then
    Exit;

  Result := False;

  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  if GetNativeValue(x) = GetNativeValue(y) then
    Exit;

  if GetNativeValue(x) <> GetNativeValue(y) then
  begin
    AddLogEntry('CompareNativeValues', e, m);
    slSuggestedTags.Add(g_sTag);
    Result := True;
  end;
end;


function SortedArrayElementByValue(e: IInterface; asPath: string; asValue: string): IInterface;
var
  i      : integer;
  kEntry : IInterface;
begin
  Result := nil;
  for i := 0 to Pred(ElementCount(e)) do
  begin
    kEntry := ElementByIndex(e, i);
    if SameText(GetElementEditValues(kEntry, asPath), asValue) then
    begin
      Result := kEntry;
      Exit;
    end;
  end;
end;


// TODO: natively implemented in 4.1.4
procedure StringListDifference(aSetListA: TStringList; aSetListB: TStringList; aLH: TStringList);
var
  i : integer;
begin
  for i := 0 to Pred(aSetListA.Count) do
    if aSetListB.IndexOf(aSetListA[i]) = -1 then
      aLH.Add(aSetListA[i]);
end;


// TODO: natively implemented in 4.1.4
procedure StringListIntersection(aSetListA: TStringList; aSetListB: TStringList; aLH: TStringList);
var
  i : integer;
begin
  for i := 0 to Pred(aSetListA.Count) do
    if aSetListB.IndexOf(aSetListA[i]) > -1 then
      aLH.Add(aSetListA[i]);
end;


// TODO: speed this up!
function IsEmptyKey(asSortKey: string): boolean;
var
  i : integer;
begin
  Result := True;
  for i := 1 to Length(asSortKey) do
    if asSortKey[i] = '1' then
    begin
      Result := False;
      Exit;
    end;
end;


function FormatTags(aslTags: TStringList; asSingular: string; asPlural: string; asNull: string): string;
var
  iTagCount : integer;
  sTagCount : string;
begin
  iTagCount := aslTags.Count;
  sTagCount := IntToStr(iTagCount);

  if iTagCount = 1 then
    Result := sTagCount + ' ' + asSingular + #13#10#32#32#32#32#32#32
  else
  if iTagCount > 1 then
    Result := sTagCount + ' ' + asPlural + #13#10#32#32#32#32#32#32;

  if iTagCount > 0 then
    Result := Result + Format(' {{BASH:%s}}', [aslTags.DelimitedText])
  else
    Result := asNull;
end;


function TagExists(asTag: string): boolean;
begin
  Result := (slSuggestedTags.IndexOf(asTag) <> -1);
end;


procedure Evaluate(e: IInterface; m: IInterface);
begin
  // exit if the tag already exists
  if TagExists(g_sTag) then
    Exit;

  // Suggest tag if one element exists while the other does not
  if CompareAssignment(e, m) then
    Exit;

  // exit if the first element does not exist
  if not Assigned(e) then
    Exit;

  // suggest tag if the two elements are different
  if CompareElementCount(e, m) then
    Exit;

  // suggest tag if the edit values of the two elements are different
  if CompareEditValue(e, m) then
    Exit;

  // compare any number of elements with CompareKeys
  if CompareKeys(e, m) then
    Exit;
end;


procedure EvaluateAdd(e: IInterface; m: IInterface);
begin
  // exit if the tag already exists
  if TagExists(g_sTag) then
    Exit;

  // exit if the first element does not exist
  if not Assigned(e) then
    Exit;

  // suggest tag if the two elements are different
  if CompareElementCountAdd(e, m) then
    Exit;
end;


procedure EvaluateChange(e: IInterface; m: IInterface);
begin
  // exit if the tag already exists
  if TagExists(g_sTag) then
    Exit;

  // exit if the first element does not exist
  if not Assigned(e) then
    Exit;

  // suggest tag if the two elements and their descendants have different contents
  if CompareKeys(e, m) then
    Exit;
end;


procedure EvaluateRemove(e: IInterface; m: IInterface);
begin
  // exit if the tag already exists
  if TagExists(g_sTag) then
    Exit;

  // exit if the first element does not exist
  if not Assigned(e) then
    Exit;

  // suggest tag if the two elements are different
  if CompareElementCountRemove(e, m) then
    Exit;
end;


procedure EvaluateByPath(e: IInterface; m: IInterface; asPath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  Evaluate(x, y);
end;


procedure EvaluateByPathAdd(e: IInterface; m: IInterface; asPath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  EvaluateAdd(x, y);
end;


procedure EvaluateByPathChange(e: IInterface; m: IInterface; asPath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  EvaluateChange(x, y);
end;


procedure EvaluateByPathRemove(e: IInterface; m: IInterface; asPath: string);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  EvaluateRemove(x, y);
end;


procedure ProcessTag(asTag: string; e: IInterface; m: IInterface);
var
  x          : IInterface;
  y          : IInterface;
  a          : IInterface;
  b          : IInterface;
  j          : IInterface;
  k          : IInterface;
  sSignature : string;
begin
  g_sTag := asTag;

  if TagExists(g_sTag) then
    Exit;

  sSignature := Signature(e);

  // Bookmark: Actors.ACBS
  if (g_sTag = 'Actors.ACBS') then
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
  else if (g_sTag = 'Actors.AIData') then
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
  else if (g_sTag = 'Actors.AIPackages') then
    EvaluateByPath(e, m, 'Packages')

  // Bookmark: Actors.Anims
  else if (g_sTag = 'Actors.Anims') then
    EvaluateByPath(e, m, 'KFFZ')

  // Bookmark: Actors.CombatStyle
  else if (g_sTag = 'Actors.CombatStyle') then
    EvaluateByPath(e, m, 'ZNAM')

  // Bookmark: Actors.DeathItem
  else if (g_sTag = 'Actors.DeathItem') then
    EvaluateByPath(e, m, 'INAM')

  // Bookmark: Actors.Perks.Add (TES5, SSE)
  else if (g_sTag = 'Actors.Perks.Add') then
    EvaluateByPathAdd(e, m, 'Perks')

  // Bookmark: Actors.Perks.Change (TES5, SSE)
  else if (g_sTag = 'Actors.Perks.Change') then
    EvaluateByPathChange(e, m, 'Perks')

  // Bookmark: Actors.Perks.Remove (TES5, SSE)
  else if (g_sTag = 'Actors.Perks.Remove') then
    EvaluateByPathRemove(e, m, 'Perks')

  // Bookmark: Actors.RecordFlags (!FO4)
  else if (g_sTag = 'Actors.RecordFlags') then
    EvaluateByPath(e, m, 'Record Header\Record Flags')

  // Bookmark: Actors.Skeleton
  else if (g_sTag = 'Actors.Skeleton') then
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
  else if (g_sTag = 'Actors.Spells') then
    EvaluateByPath(e, m, 'Spells')

  // Bookmark: Actors.Stats
  else if (g_sTag = 'Actors.Stats') then
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
  else if (g_sTag = 'Actors.Voice') then
    EvaluateByPath(e, m, 'VTCK')

  // Bookmark: C.Acoustic
  else if (g_sTag = 'C.Acoustic') then
    EvaluateByPath(e, m, 'XCAS')

  // Bookmark: C.Climate
  else if (g_sTag = 'C.Climate') then
  begin
    // add tag if the Behave Like Exterior flag is set ine one record but not the other
    if CompareFlags(e, m, 'DATA', 'Behave Like Exterior', True, True) then
      Exit;

    // evaluate additional property
    EvaluateByPath(e, m, 'XCCM');
  end

  // Bookmark: C.Encounter
  else if (g_sTag = 'C.Encounter') then
    EvaluateByPath(e, m, 'XEZN')

  // Bookmark: C.ForceHideLand (!TES4, !FO4)
  else if (g_sTag = 'C.ForceHideLand') then
    EvaluateByPath(e, m, 'XCLC\Land Flags')

  // Bookmark: C.ImageSpace
  else if (g_sTag = 'C.ImageSpace') then
    EvaluateByPath(e, m, 'XCIM')

  // Bookmark: C.Light
  else if (g_sTag = 'C.Light') then
    EvaluateByPath(e, m, 'XCLL')

  // Bookmark: C.Location
  else if (g_sTag = 'C.Location') then
    EvaluateByPath(e, m, 'XLCN')

  // Bookmark: C.LockList
  else if (g_sTag = 'C.LockList') then
    EvaluateByPath(e, m, 'XILL')

  // Bookmark: C.MiscFlags (!FO4)
  else if (g_sTag = 'C.MiscFlags') then
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
  else if (g_sTag = 'C.Music') then
    EvaluateByPath(e, m, 'XCMO')

  // Bookmark: FULL (C.Name, Names, SpellStats)
  else if ContainsStr(g_sTag, 'C.Name Names SpellStats') then
    EvaluateByPath(e, m, 'FULL')

  // Bookmark: C.Owner
  else if (g_sTag = 'C.Owner') then
    EvaluateByPath(e, m, 'Ownership')

  // Bookmark: C.RecordFlags
  else if (g_sTag = 'C.RecordFlags') then
    EvaluateByPath(e, m, 'Record Header\Record Flags')

  // Bookmark: C.Regions
  else if (g_sTag = 'C.Regions') then
    EvaluateByPath(e, m, 'XCLR')

  // Bookmark: C.SkyLighting
  // add tag if the Behave Like Exterior flag is set in one record but not the other
  else if (g_sTag = 'C.SkyLighting') and CompareFlags(e, m, 'DATA', 'Use Sky Lighting', True, True) then
    Exit

  // Bookmark: C.Water
  else if (g_sTag = 'C.Water') then
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
  else if (g_sTag = 'Creatures.Blood') then
  begin
    EvaluateByPath(e, m, 'NAM0');
    EvaluateByPath(e, m, 'NAM1');
  end

  // Bookmark: Creatures.Type
  else if (g_sTag = 'Creatures.Type') then
    EvaluateByPath(e, m, 'DATA\Type')

  // Bookmark: Deflst
  else if (g_sTag = 'Deflst') then
    EvaluateByPathRemove(e, m, 'FormIDs')

  // Bookmark: Destructible
  else if (g_sTag = 'Destructible') then
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
  else if (g_sTag = 'EffectStats') then
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
  else if (g_sTag = 'EnchantmentStats') then
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
  else if (g_sTag = 'Factions') then
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
  else if (g_sTag = 'Graphics') then
  begin
    // evaluate Icon and Model properties
    if ContainsStr(sSignature, 'ALCH AMMO APPA BOOK INGR KEYM LIGH MGEF MISC SGST SLGM TREE WEAP') then
    begin
      EvaluateByPath(e, m, 'Icon');
      EvaluateByPath(e, m, 'Model');
    end

    // evaluate Icon properties
    else if ContainsStr(sSignature, 'BSGN CLAS LSCR LTEX REGN') then
      EvaluateByPath(e, m, 'Icon')

    // evaluate Model properties
    else if ContainsStr(sSignature, 'ACTI DOOR FLOR FURN GRAS STAT') then
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
  else if (g_sTag = 'Invent.Add') then
    EvaluateByPathAdd(e, m, 'Items')

  // Bookmark: Invent.Change - TEST
  else if (g_sTag = 'Invent.Change') then
    EvaluateByPathChange(e, m, 'Items')

  // Bookmark: Invent.Remove
  else if (g_sTag = 'Invent.Remove') then
    EvaluateByPathRemove(e, m, 'Items')

  // Bookmark: Keywords
  else if (g_sTag = 'Keywords') then
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
  else if (g_sTag = 'NPC.AIPackageOverrides') then
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
  else if (g_sTag = 'NPC.AttackRace') then
    EvaluateByPath(e, m, 'ATKR')

  // Bookmark: NPC.Class
  else if (g_sTag = 'NPC.Class') then
    EvaluateByPath(e, m, 'CNAM')

  // Bookmark: NPC.CrimeFaction
  else if (g_sTag = 'NPC.CrimeFaction') then
    EvaluateByPath(e, m, 'CRIF')

  // Bookmark: NPC.DefaultOutfit
  else if (g_sTag = 'NPC.DefaultOutfit') then
    EvaluateByPath(e, m, 'DOFT')

  // Bookmark: NPC.Eyes
  else if (g_sTag = 'NPC.Eyes') then
    EvaluateByPath(e, m, 'ENAM')

  // Bookmark: NPC.FaceGen
  else if (g_sTag = 'NPC.FaceGen') then
    EvaluateByPath(e, m, 'FaceGen Data')

  // Bookmark: NPC.Hair
  else if (g_sTag = 'NPC.Hair') then
    EvaluateByPath(e, m, 'HNAM')

  // Bookmark: NPC.Race
  else if (g_sTag = 'NPC.Race') then
    EvaluateByPath(e, m, 'RNAM')

  // Bookmark: ObjectBounds
  else if (g_sTag = 'ObjectBounds') then
    EvaluateByPath(e, m, 'OBND')

  // Bookmark: Outfits.Add
  else if (g_sTag = 'Outfits.Add') then
    EvaluateByPathAdd(e, m, 'OTFT')

  // Bookmark: Outfits.Remove
  else if (g_sTag = 'Outfits.Remove') then
    EvaluateByPathRemove(e, m, 'OTFT')

  // Bookmark: R.AddSpells - DEFER: R.ChangeSpells

  // Bookmark: R.Attributes-F
  else if (g_sTag = 'R.Attributes-F') then
    EvaluateByPath(e, m, 'ATTR\Female')

  // Bookmark: R.Attributes-M
  else if (g_sTag = 'R.Attributes-M') then
    EvaluateByPath(e, m, 'ATTR\Male')

  // Bookmark: R.Body-F
  else if (g_sTag = 'R.Body-F') then
    EvaluateByPath(e, m, 'Body Data\Female Body Data\Parts')

  // Bookmark: R.Body-M
  else if (g_sTag = 'R.Body-M') then
    EvaluateByPath(e, m, 'Body Data\Male Body Data\Parts')

  // Bookmark: R.Body-Size-F
  else if (g_sTag = 'R.Body-Size-F') then
  begin
    EvaluateByPath(e, m, 'DATA\Female Height');
    EvaluateByPath(e, m, 'DATA\Female Weight');
  end

  // Bookmark: R.Body-Size-M
  else if (g_sTag = 'R.Body-Size-M') then
  begin
    EvaluateByPath(e, m, 'DATA\Male Height');
    EvaluateByPath(e, m, 'DATA\Male Weight');
  end

  // Bookmark: R.ChangeSpells
  else if (g_sTag = 'R.ChangeSpells') then
    EvaluateByPath(e, m, 'Spells')

  // Bookmark: R.Description
  else if (g_sTag = 'R.Description') then
    EvaluateByPath(e, m, 'DESC')

  // Bookmark: R.Ears
  else if (g_sTag = 'R.Ears') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[1]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[1]');
  end

  // Bookmark: R.Eyes
  else if (g_sTag = 'R.Eyes') then
    EvaluateByPath(e, m, 'ENAM')

  // Bookmark: R.Hair
  else if (g_sTag = 'R.Hair') then
    EvaluateByPath(e, m, 'HNAM')

  // Bookmark: R.Head
  else if (g_sTag = 'R.Head') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[0]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[0]');
    EvaluateByPath(e, m, 'FaceGen Data');
  end

  // Bookmark: R.Mouth
  else if (g_sTag = 'R.Mouth') then
  begin
    EvaluateByPath(e, m, 'Head Data\Male Head Data\Parts\[2]');
    EvaluateByPath(e, m, 'Head Data\Female Head Data\Parts\[2]');
  end

  // Bookmark: R.Relations.Add
  else if (g_sTag = 'R.Relations.Add') then
    EvaluateByPathAdd(e, m, 'Relations')

  // Bookmark: R.Relations.Change - TEST
  else if (g_sTag = 'R.Relations.Change') then
    EvaluateByPathChange(e, m, 'Relations')

  // Bookmark: R.Relations.Remove
  else if (g_sTag = 'R.Relations.Remove') then
    EvaluateByPathRemove(e, m, 'Relations')

  // Bookmark: R.Skills
  else if (g_sTag = 'R.Skills') then
    EvaluateByPath(e, m, 'DATA\Skill Boosts')

  // Bookmark: R.Teeth
  else if (g_sTag = 'R.Teeth') then
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
  else if (g_sTag = 'R.Voice-F') then
    EvaluateByPath(e, m, 'VTCK\Voice #1 (Female)')

  // Bookmark: R.Voice-M
  else if (g_sTag = 'R.Voice-M') then
    EvaluateByPath(e, m, 'VTCK\Voice #0 (Male)')

  // Bookmark: Relations.Add
  else if (g_sTag = 'Relations.Add') then
    EvaluateByPathAdd(e, m, 'Relations')

  // Bookmark: Relations.Change - TEST
  else if (g_sTag = 'Relations.Change') then
    EvaluateByPathChange(e, m, 'Relations')

  // Bookmark: Relations.Remove
  else if (g_sTag = 'Relations.Remove') then
    EvaluateByPathRemove(e, m, 'Relations')

  // Bookmark: Roads
  else if (g_sTag = 'Roads') then
    EvaluateByPath(e, m, 'PGRP')

  // Bookmark: Scripts
  else if (g_sTag = 'Scripts') then
    EvaluateByPath(e, m, 'SCRI')

  // Bookmark: Sound
  else if (g_sTag = 'Sound') then
  begin
    // Activators, Containers, Doors, and Lights
    if ContainsStr(sSignature, 'ACTI CONT DOOR LIGH') then
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
      else then
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
  else if (g_sTag = 'SpellStats') then
    EvaluateByPath(e, m, 'SPIT')

  // Bookmark: Stats
  else if (g_sTag = 'Stats') then
  begin
    if ContainsStr(sSignature, 'ALCH AMMO APPA ARMO BOOK CLOT INGR KEYM LIGH MISC SGST SLGM WEAP') then
    begin
      EvaluateByPath(e, m, 'EDID');
      EvaluateByPath(e, m, 'DATA');

      if ContainsStr(sSignature, 'ARMO WEAP') then
        EvaluateByPath(e, m, 'DNAM')

      else if sSignature = 'WEAP' then
        EvaluateByPath(e, m, 'CRDT');
    end

    else if sSignature = 'ARMA' then
      EvaluateByPath(e, m, 'DNAM');
  end

  // Bookmark: Text
  else if (g_sTag = 'Text') then
  begin
    if ContainsStr(sSignature, 'ALCH AMMO APPA ARMO AVIF BOOK BSGN CHAL CLAS IMOD LSCR MESG MGEF PERK SCRL SHOU SKIL SPEL TERM WEAP') then
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
  else if (g_sTag = 'WeaponMods') then
    EvaluateByPath(e, m, 'Weapon Mods');
end;


procedure ProcessDelevRelevTags(e: IInterface; m: IInterface);
var
  kEntries       : IInterface;
  kEntriesMaster : IInterface;
  kEntry         : IInterface;
  kEntryMaster   : IInterface;
  kCOED          : IInterface; // extra data
  kCOEDMaster    : IInterface; // extra data
  sSignature     : string;
  sSortKey       : string;
  sSortKeyMaster : string;
  i              : integer;
  j              : integer;
begin
  // nothing to do if already tagged
  if TagExists('Delev') and TagExists('Relev') then
    Exit;

  // get Leveled List Entries
  kEntries := ElementByName(e, 'Leveled List Entries');
  kEntriesMaster := ElementByName(m, 'Leveled List Entries');

  if not Assigned(kEntries)
  or not Assigned(kEntriesMaster) then
    Exit;

  // initalize count matched on reference entries
  j := 0;

  // iterate through all entries
  g_sTag := 'Relev';
  for i := 0 to Pred(ElementCount(kEntries)) do
  begin
    kEntry := ElementByIndex(kEntries, i);
    kEntryMaster := SortedArrayElementByValue(kEntriesMaster, 'LVLO\Reference', GetElementEditValues(kEntry, 'LVLO\Reference'));

    if not Assigned(kEntryMaster) then
      Continue;

    Inc(j);

    if TagExists(g_sTag) then
      Continue;

    if CompareNativeValues(kEntry, kEntryMaster, 'LVLO\Level')
    or CompareNativeValues(kEntry, kEntryMaster, 'LVLO\Count') then
      Exit;

    if wbIsOblivion then
      Continue;

    // Relev check for changed level, count, extra data
    kCOED := ElementBySignature(kEntry, 'COED');
    kCOEDMaster := ElementBySignature(kEntryMaster, 'COED');

    sSortKey := SortKeyEx(kCOED);
    sSortKeyMaster := SortKeyEx(kCOEDMaster);

    if not SameText(sSortKey, sSortKeyMaster) then
    begin
      AddLogEntry('Assigned', sSortKey, sSortKeyMaster);
      slSuggestedTags.Add(g_sTag);
      Exit;
    end;
  end;

  sSignature := Signature(e);

  // if number of matched entries less than in master list
  g_sTag := 'Delev';

  if (((sSignature = 'LVLC') and (wbIsOblivion or wbIsFallout3 or wbIsFalloutNV))
  or (sSignature = 'LVLI') or ((sSignature = 'LVLN') and not wbIsOblivion)
  or ((sSignature = 'LVSP') and (wbIsOblivion or wbIsSkyrim)))
  and not TagExists(g_sTag) then
    if j < ElementCount(kEntriesMaster) then
    begin
      AddLogEntry('ElementCount', kEntries, kEntriesMaster);
      slSuggestedTags.Add(g_sTag);
      Exit;
    end;
end;

function AddLogEntry(asTestName: string; e: IInterface; m: IInterface): string;
var
  mr    : IwbMainRecord;
  sName : string;
  sPath : string;
begin
  if optionOutputLog = mrNo then
    Exit;

  if Assigned(m) then
  begin
    mr := ContainingMainRecord(m);
    sPath := Path(m);
  end else
  begin
    mr := ContainingMainRecord(e);
    sPath := Path(e);
  end;

  sPath := RightStr(sPath, Length(sPath) - 5);

  sName := Format('[%s:%s]', [Signature(mr), IntToHex(GetLoadOrderFormID(mr), 8)]);

  slLog.Add(Format('{%s} (%s) %s %s', [g_sTag, asTestName, sName, sPath]));
end;


function FileByName(asFileName: string): IwbFile;
var
  kFile : IwbFile;
  i     : integer;
begin
  Result := nil;

  for i := 0 to Pred(FileCount) do
  begin
    kFile := FileByIndex(i);
    if asFileName = GetFileName(kFile) then
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
  if optionAddTags = mrYes then
    optionAddTags := mrNo
  else
    optionAddTags := mrYes;
end;


procedure chkLoggingClick(Sender: TObject);
begin
  if optionOutputLog = mrYes then
    optionOutputLog := mrNo
  else
    optionOutputLog := mrYes;
end;


function Configure(asCaption: string): IwbFile;
var
  frm        : TForm;
  lblPlugins : TLabel;
  chkAddTags : TCheckBox;
  chkLogging : TCheckBox;
  cbbPlugins : TComboBox;
  btnCancel  : TButton;
  btnOk      : TButton;
  i          : integer;

  kFile      : IwbFile;
begin
  Result := nil;

  frm := TForm.Create(TForm(frmMain));

  try
    frm.Caption      := asCaption;
    frm.BorderStyle  := bsToolWindow;
    frm.ClientWidth  := 234 * scaleFactor;
    frm.ClientHeight := 154 * scaleFactor;
    frm.Position     := poScreenCenter;
    frm.KeyPreview   := True;
    frm.OnKeyDown    := EscKeyHandler;

    lblPlugins := TLabel.Create(frm);
    lblPlugins.Parent   := frm;
    lblPlugins.Left     := 16 * scaleFactor;
    lblPlugins.Top      := 64 * scaleFactor;
    lblPlugins.Width    := 200 * scaleFactor;
    lblPlugins.Height   := 16 * scaleFactor;
    lblPlugins.Caption  := 'Select file to analyze:';
    lblPlugins.AutoSize := False;

    chkAddTags := TCheckBox.Create(frm);
    chkAddTags.Parent   := frm;
    chkAddTags.Left     := 16 * scaleFactor;
    chkAddTags.Top      := 16 * scaleFactor;
    chkAddTags.Width    := 185 * scaleFactor;
    chkAddTags.Height   := 16 * scaleFactor;
    chkAddTags.Caption  := 'Write suggested tags to header';
    chkAddTags.Checked  := False;
    chkAddTags.OnClick  := chkAddTagsClick;
    chkAddTags.TabOrder := 0;

    chkLogging := TCheckBox.Create(frm);
    chkLogging.Parent   := frm;
    chkLogging.Left     := 16 * scaleFactor;
    chkLogging.Top      := 39 * scaleFactor;
    chkLogging.Width    := 185 * scaleFactor;
    chkLogging.Height   := 16 * scaleFactor;
    chkLogging.Caption  := 'Log test results to Messages tab';
    chkLogging.Checked  := True;
    chkLogging.OnClick  := chkLoggingClick;
    chkLogging.TabOrder := 1;

    cbbPlugins := TComboBox.Create(frm);
    cbbPlugins.Parent         := frm;
    cbbPlugins.Left           := 16 * scaleFactor;
    cbbPlugins.Top            := 85 * scaleFactor;
    cbbPlugins.Width          := 200 * scaleFactor;
    cbbPlugins.Height         := 21 * scaleFactor;
    cbbPlugins.Style          := csDropDownList;
    cbbPlugins.DoubleBuffered := True;
    cbbPlugins.TabOrder       := 2;

    for i := 0 to Pred(FileCount) do
    begin
      kFile := FileByIndex(i);
      if IsEditable(kFile) then
        cbbPlugins.Items.Add(GetFileName(kFile));
    end;

    cbbPlugins.ItemIndex      := Pred(cbbPlugins.Items.Count);

    btnOk := TButton.Create(frm);
    btnOk.Parent              := frm;
    btnOk.Left                := 62 * scaleFactor;
    btnOk.Top                 := 120 * scaleFactor;
    btnOk.Width               := 75 * scaleFactor;
    btnOk.Height              := 25 * scaleFactor;
    btnOk.Caption             := 'Run';
    btnOk.Default             := True;
    btnOk.ModalResult         := mrOk;
    btnOk.TabOrder            := 3;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent          := frm;
    btnCancel.Left            := 143 * scaleFactor;
    btnCancel.Top             := 120 * scaleFactor;
    btnCancel.Width           := 75 * scaleFactor;
    btnCancel.Height          := 25 * scaleFactor;
    btnCancel.Caption         := 'Abort';
    btnCancel.ModalResult     := mrAbort;
    btnCancel.TabOrder        := 4;

    if frm.ShowModal = mrOk then
      Result := FileByName(cbbPlugins.Text);
  finally
    frm.Free;
  end;
end;

end.