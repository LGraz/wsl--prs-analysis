- [x] 2.5
- [x] 3.4 
- [x] Fig. 3
- [x] Fig. 5
- [x] Fig. 6
- [x] Fig. 7
- [ ] Supplementary Material
  - [ ] remove legacy stuff
- [ ] Upload new plots


### General Comments for Manuscript
- An manchen Stellen habe ich "out-of-sample" hinzugefügt. Dies ist wirklich relevant, da häufig "in-sample" prediction quality rapportiert wird, welche meist viel zu hoch und nicht aussagekräftig ist.
- Tabelle 5 finde ich noch relevant, da die Ergebnisse Ende Sektion 3.4 qualitativ besprochen werden und es hilfreich ist, die genauen Werte abzulesen. Tabelle 3 und 6 tragen weniger zum Mehrwert bei, meiner Meinung nach. 

### Supplementary material
- Given that we archived the code on the permanent storage Xenodo, one could think about removing code completely. I do not have a preference.
- For an unknown reason, I was unable to open the supplement in the web version of Word (and I cannot use it otherwise since Microsoft does not support Linux). Hence I modified it in LibreOffice, and I hope that the formatting did not suffer too much. Attached you find the file with my tracked modifications. I mostly removed unnecessary sections here for redundancy:
  - Section 3.3 I removed "Legacy Code" completely, as it is not used anymore. 
  - I removed 4.4 and 4.5 as they are well enough summarized by table 5 and 6 (as you suggested in the email)
  - Removed in particular 4.5.6 "Legacy codes" as this is no longer used
- @Silvia, can you add table 6 (and 5 if also removed from the manuscript) to the supplementary material and link accordingly in the manuscript?


### FIGURES  
Folgende Edits schlage ich für die Figures vor: Ich habe es schnell zur Vorschau implementiert. Gerne kann ich Teile davon rückgängig machen. https://lgraz.com/wsl--prs-analysis/notebooks/noise-plots-preview.html 
1. Fig. 6 und 7: vertikale graue Noise-group linien entfernt, da es sich um RL_NOISE handelt. Noise Groups sind definiert via HM_NOISE, soweit ich weiss. Ansonsten sollte man die Gruppierung auch in Figure5 hinzufügen.
2. In Fig 5 sehe ich, dass du entsprechende Captions hinzugefügt hast. Ich habe diese nativ in die Graphen integriert.
3. Fig 7: Ich habe BA, EC ... in der Legende ausgeschrieben, somit kann man die caption vereinfachen. Wir sollten uns aber entscheiden, ob wir dem Leser zumuten, die Abkürzungen zu kennen. So müsste man Tabelle 5, die spaltenname/caption anpassen.
4. Fig. 5 und 7 haben die selbe struktur. Ich habe beide mal zusammengeführt. Damit können wir die obere Grenze von 10 einhalten, ohne Tabelle 5 zu verlieren. Für die Konsistenz sind jetzt alle y-Achsen-Limits dynamisch gewählt (sonst würde man für die PRS-Werte nur gerade Linien sehen).
5. Fig. 6 ist jetzt lesbarer. (blau stadt orange, und die linie schwarz statt blau)


### Website
- as LOC_* variables are again included for the hypothesis testing, I reverted the corresponding changes on the website.
- For prediction, we now show both, mediators with and without LOC_ variables. Both results are now shown on the website, s.t. you do not have to look for archived material.


