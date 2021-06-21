# Wrye Bash Tag Generator

xEdit script for generating tags for Wrye Bash

## Unsupported tags

All tags are supported except those that require the user's explicit intent.

*(See: [Wrye Bash Advanced Readme](https://wrye-bash.github.io/docs/Wrye%20Bash%20Advanced%20Readme.html))*

### Special Function Tags

- `Deactivate`
- `Filter`
- `IIM`
- `MustBeActiveIfImported`
- `NoMerge`

### Other Tags

- `Actors.AIPackagesForceAdd`
- `Actors.SpellsForceAdd`
- `NpcFacesForceFullImport`
- `R.AddSpells`


## New tags

The following tags were added or changed in v1.6.4.0.

### Added tags

New Tag | Supported Games
:--- | :---
`Actors.Perks.Add` | TES5, SSE
`Actors.Perks.Change` | TES5, SSE
`Actors.Perks.Remove` | TES5, SSE
`Actors.Voice` | FO3, FNV, TES5, SSE
`C.ForceHideLand` | FO3, FNV, TES5, SSE
`C.MiscFlags` | FO3, FNV, TES4, TES5, SSE
`Creatures.Type` | FO3, FNV, TES4
`Deflst` | FO3, FNV
`EffectStats` | FO3, FNV, TES4, TES5, SSE
`EnchantmentStats` | FO3, FNV, TES4, TES5, SSE
`Factions` | FO3, FNV, TES4, TES5, SSE
`NPC.AIPackageOverrides` | TES5, SSE
`NPC.AttackRace` | TES5, SSE
`NPC.CrimeFaction` | TES5, SSE
`NPC.DefaultOutfit` | TES5, SSE
`NPC.Eyes` | FO3, FNV, TES4
`NPC.FaceGen` | FO3, FNV, TES4
`NPC.Hair` | FO3, FNV, TES4
`Outfits.Add` | TES5, SSE
`Outfits.Remove` | TES5, SSE


### Replaced tags

Old Tag | Split Into | Supported Games
:--- | :--- | :---
`Invent` | `Invent.Add`<br>`Invent.Change`<br>`Invent.Remove` | FO3, FNV, TES4, TES5, SSE
`R.Relations` | `R.Relations.Add`<br>`R.Relations.Change`<br>`R.Relations.Remove` | FO3, FNV, TES4
`Relations` | `Relations.Add`<br>`Relations.Change`<br>`Relations.Remove` | FO3, FNV, TES4, TES5, SSE


### Renamed tags

Old Tag | New Tag | Supported Games
:--- | :--- | :---
`Body-F` | `R.Body-F` | FO3, FNV, TES4
`Body-M` | `R.Body-M` | FO3, FNV, TES4
`Body-Size-F` | `R.Body-Size-F` | FO3, FNV, TES4
`Body-Size-M` | `R.Body-Size-M` | FO3, FNV, TES4
`Eyes` | `R.Eyes` | FO3, FNV, TES4
`Hair` | `R.Hair` | FO3, FNV, TES4
