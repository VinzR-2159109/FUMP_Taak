# FUMP_TAAK
## 🏡 Taak 1: HomeSwapping

Deze taak genereert en lost een woningruilprobleem op.

### Uitvoeren

```bash
cd HomeSwapping
cabal run homeswapping -- <n> <min_c> <max_c> <output.yaml>
````

### Voorbeeld

```bash
cabal run homeswapping -- 5 2 3 input.yaml
```

Dit genereert een probleem met 5 knopen, minimaal 2 en maximaal 3 verbindingen per knoop, slaat dit op in `input.yaml` en berekent de optimale ruilcombinaties.

---

## 🌙 Taak 2: MaanLander

Een GUI-gebaseerde simulator van een maanlanding, met behulp van Threepenny-GUI.

### Uitvoeren

```bash
cd MaanLander
cabal run
```

Open vervolgens je browser op:
`http://localhost:8023`
Gebruik de sliders en dropdowns om je simulatie te configureren en klik op "Simulate" om het resultaat te bekijken.

---

## 🧠 Taak 3: TypeClasses

Een polymorf vergelijkingssysteem met aangepaste datatypes.

### Uitvoeren

```bash
cd TypeClasses
cabal run
```

Dit toont de afstanden, gelijkenissen en connectiviteit tussen personen, bomen en afbeeldingen met behulp van zelfgedefinieerde metrieken en vergelijkingsfuncties.

---

## 📁 Mappenstructuur

```
FUMP_TAAK/
├── HomeSwapping/     # Taak 1
├── MaanLander/       # Taak 2
├── TypeClasses/      # Taak 3
└── .gitignore
```
