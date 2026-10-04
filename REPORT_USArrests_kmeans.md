# Report di coordinamento: clustering k-means di `USArrests`

## Obiettivo e ruoli

L'analisi raggruppa i 50 Stati statunitensi del dataset `USArrests` in base a
quattro indicatori (`Murder`, `Assault`, `UrbanPop` e `Rape`). L'agente analista
ha predisposto lo script R; l'agente coordinatore ne ha controllato metodo,
sintassi e artefatti attesi.

## Metodo

- Lo script usa il dataset incluso nel pacchetto R `datasets`, senza dipendenze
  esterne.
- Le quattro variabili sono standardizzate con `scale()`. Questo passaggio è
  necessario perché il k-means usa distanze euclidee e le variabili originali
  hanno scale molto diverse.
- Sono confrontate le soluzioni con `k` da 2 a 10. Per ciascun valore viene
  eseguito `kmeans()` con `nstart = 100` e `iter.max = 100`.
- La riproducibilità è controllata da un seed base pari a `20261004`; per ogni
  soluzione viene impostato il seed `20261004 + k`.
- La scelta di `k` è definita *a priori* come il massimo coefficiente silhouette
  medio. WSS totale e quota di devianza tra cluster sul totale sono esportate
  come diagnostiche complementari, utili anche per valutare l'eventuale
  “gomito”. La silhouette è calcolata direttamente in base R e vale zero per
  un cluster singleton.

## Esito del controllo

La revisione statica conferma che lo script:

1. applica il clustering ai dati standardizzati;
2. conserva esattamente il fit associato al massimo della silhouette media;
3. riconverte correttamente i centroidi nelle unità originali, moltiplicando
   per le deviazioni standard e sommando le medie usate da `scale()`;
4. esporta diagnostiche, assegnazioni Stato-cluster, centroidi standardizzati,
   centroidi originali e un riepilogo testuale nella directory
   `results/usarrests_kmeans/`.

Non è stato possibile produrre o verificare risultati numerici in questo
ambiente, perché gli eseguibili `R` e `Rscript` non sono installati. Anche il
tentativo dell'analista di installare `r-base-core` non è riuscito: il proxy ha
risposto HTTP 403 ai repository configurati e il pacchetto non è risultato
disponibile. Di conseguenza, **non si riportano come osservati** né il valore di
`k`, né composizione, dimensioni o profili dei cluster. Questi risultati saranno
determinati in modo riproducibile al primo avvio dello script in un ambiente R.

## Lettura dei risultati dopo l'esecuzione

Dopo l'esecuzione, il valore selezionato di `k`, silhouette, WSS, quota di
devianza spiegata, dimensioni e centroidi sono raccolti in `summary.txt`.
La composizione si legge da `assignments.csv`, raggruppando gli Stati per
`cluster`. I profili vanno descritti confrontando i centroidi in
`centers_original_units.csv`: valori elevati o bassi di omicidi, aggressioni,
urbanizzazione e stupri caratterizzano ciascun gruppo nelle unità interpretabili
del dataset. `centers_scaled.csv` consente invece di confrontare direttamente
l'intensità relativa delle quattro dimensioni.

I numeri dei cluster non hanno significato ordinale e possono essere permutati:
le interpretazioni devono quindi riferirsi ai centroidi e agli Stati membri,
non all'etichetta numerica.

## Limiti

- Il k-means privilegia gruppi compatti e approssimativamente sferici e può non
  rappresentare strutture non lineari o cluster di forma/densità diversa.
- Il criterio silhouette è un criterio operativo, non una dimostrazione che
  esista un unico numero “vero” di cluster; WSS e stabilità rispetto a seed,
  intervallo di `k` e ricampionamento meritano un controllo di sensibilità.
- Il dataset contiene solo 50 osservazioni e quattro indicatori aggregati; il
  clustering è descrittivo, non causale.
- Outlier e correlazioni tra variabili possono influire sulle distanze. La
  standardizzazione riequilibra le scale, ma non elimina tali effetti.
- L'esatta riproducibilità numerica può risentire della versione di R e delle
  librerie matematiche, sebbene seed, inizializzazioni multiple e parametri siano
  fissati.

## Riproducibilità

Dalla radice del repository, in un ambiente con R installato:

```bash
Rscript usarrests_kmeans.R
cat results/usarrests_kmeans/summary.txt
```

Per un controllo minimo degli artefatti:

```bash
test -s results/usarrests_kmeans/diagnostics.csv
test -s results/usarrests_kmeans/assignments.csv
test -s results/usarrests_kmeans/centers_scaled.csv
test -s results/usarrests_kmeans/centers_original_units.csv
test -s results/usarrests_kmeans/summary.txt
```
