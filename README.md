# sanskrit metrics
emacs lisp functions to analyze common metres in Sanskrit

## Provided Functions
This package currently provides the following interactive functions
- **sktmetrics-context** : analyze a string in region or in a recognized stanza environment metrically

## Installation
1. Add `sanskrit-metrics.el` to a directory included in the list `load-path` or modify this list to include the directory with
   `(add-to-list 'load-path "/PATH/TO/sanskrit-metrics/")` in your init file,
2. add `(require 'sanskrit-metrics)` to your init-file,
3. (optional) append lists with markup you want to include with `(setq LISTNAME (append LISTNAME '("additional string 1" "additional string 2" "…")))` in your init-file; the following lists can be appended:
   - *sktm_l-beg* : beginning of recognized stanza environment
   - *sktm_l-end* : end of recognized stanza environment
   - *sktm_l-plain-elim* : plain text markup to be deleted completely before analysis is performed (doesn't affect input), mostly punctuation
   - *sktm_l-latex-elim* : LaTeX markup to be deleted completely before analysis is performed (doesn't affect input) like comments, `\footnote{…}`, and `\cite{…}`
   - *sktm_l-tei-elim* :  TEI markup to be deleted completely before analysis is performed (doesn't affect input) like comments and `<note>…</note>`
   - *sktm_l-latex-keep* : LaTeX to be reduced to plain content (doesn't affect input) like `\emph{…}`, and `\textbf{…}`
   - *sktm_l-tei-keep* : TEI markup to be reduced to plain content (doesn't affect input) like `<l>…<l/>`, `<seg>…<seg/>`, and `<hi>…<hi/>`

## Prerequisites
- anuṣṭubh stanzas with 4 pādas and āryā stanzas need to be encoded in 2 lines,
- anuṣṭubh stanzas with 6 pādas in 3 lines,
- all other metres should be encoded in 4 lines, 1 per pāda.

## Usage
1. Set region (default `M-SPC`) to include the strings you wish to analyze, otherwise the function defaults to the recognized environments specified in *sktm_l-beg* and *sktm_l-end*,
2. evaluate the above functions with `M-x sktmetrics-context`,
3. (optional) if you are frequently using this function, consider [creating a keybinding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html).

## Known issues
- the analyzis of caesurae is based on visible word boundries alone, which isn't reliable; so most of the output regarding them can be ignored

## Examples (output lines starting with `%`)

### plain text

```
divi bhūmau tathākāśe bahir antaś ca me vibhuḥ /
yo 'vabhāty avabhāsātmā tasmai viśvātmane namaḥ // MU_1,1.1
% ◡  ◡| —  —| ◡  —  —  —| % A pathyā
% ◡  ◡| —  —| ◡| —| ◡  —  % B correct
% —| ◡  —| ◡  ◡  —  —  —| % C pathyā
% —  —| —  —  ◡  —| ◡  —  % D correct
```

```
kaścit kāntāvirahaguruṇā svādhikārapramattaḥ
śāpenāstaṅgamitamahimā varṣabhogyeṇa bhartuḥ /
yakṣaś cakre janakatanayāsnānapuṇyodakeṣu
snigdhacchāyātaruṣu vasatiṃ rāmagiryāśrameṣu // KMgD_1 //
% —  —| —  —  ◡  ◡  ◡  ◡  ◡  —| —  ◡  —  —  ◡  —  —  % Mandākrāntā (4+6+7): caesura in compound or incorrect?
% —  —  —  —  ◡  ◡  ◡  ◡  ◡  —| —  ◡  —  —  ◡| —  —  % Mandākrāntā (4+6+7): caesura in compound or incorrect?
% —  —| —  —| ◡  ◡  ◡  ◡  ◡  —  —  ◡  —  —  ◡  —  —  % Mandākrāntā (4+6+7): caesura in compound or incorrect?
% —  —  —  —  ◡  ◡  ◡| ◡  ◡  —| —  ◡  —  —  ◡  —  —  % Mandākrāntā (4+6+7): caesura in compound or incorrect?
```

```
āsīt sa ko'pi jantur yenāho svīyabuddhivaikalyāt /
saṃprāpya kīṭayoniṃ caṇḍālasṛtau sukhaṃ patitam	// VRSS_10.49
% —  —| ◡| —  ◡| —  —| —  —  —| —  ◡  —  ◡  —  —  —  
% —  —  ◡| —  ◡  —  —| —  —  ◡  ◡  —| ◡  —| ◡  ◡  —  % Āryā (30+27 morae): vipulā
```

### LaTeX

```latex
begin{verse}
divi bhūmau tathākāśe\footnote{Cf. \cite{AuthorYear}.} bahir antaś ca me vibhuḥ |\\
yo 'vabhāty \emph{avabhāsātmā} tasmai viśvātmane namaḥ || 1,1.1 ||
% ◡  ◡| —  —| ◡  —  —  —| % A pathyā
% ◡  ◡| —  —| ◡| —| ◡  —  % B correct
% —| ◡  —| ◡  ◡  —  —  —| % C pathyā
% —  —| —  —  ◡  —| ◡  —  % D correct
\end{verse}
```

### TEI

```xml
<lg xml:id="MU_1.1.1">
<l><seg type="pāda" n="a">divi bhūmau tathākāśe<note>Random note.</note></seg> <seg type="pāda" n="d">bahir antaś ca me vibhuḥ</seg> /</l>
<l><seg type="pāda" n="c">yo 'vabhāty <hi>avabhāsātmā</hi></seg> <seg type="pāda" n="d">tasmai viśvātmane namaḥ</seg> //</l>
% ◡  ◡| —  —| ◡  —  —  —| % A pathyā
% ◡  ◡| —  —| ◡| —| ◡  —  % B correct
% —| ◡  —| ◡  ◡  —  —  —| % C pathyā
% —  —| —  —  ◡  —| ◡  —  % D correct
</lg>
```

## Recognized metres
- anuṣṭubh, both pathyā and vipulā
- samavṛtta:
  - indravajrā
  - upendravajrā
  - rathoddhatā
  - śālinī
  - svāgatā
  - indravaṃśā
  - toṭaka
  - drutavilambita
  - pramitākṣarā
  - vaṃśastha
  - praharṣiṇī
  - mañjubhāṣiṇī
  - rucirā
  - vasantatilaka
  - mālinī
  - nardaṭaka
  - pṛthvī
  - mandākrāntā
  - śikhariṇī
  - hariṇī
  - śārdūlavikrīḍita
  - sragdharā
- ardhasamavṛtta:
  - viyoginī
  - aparavaktra
  - mālabhāriṇī
  - puṣpitāgrā
- viṣamavṛtta:
  - udgatā
- mātrāvṛtta:
  - āryā
  - sarvataścapalā
  - mukhacapalā
  - jaghanacapalā
  - gīti
  - upagīti
  - udgīti
  - āryāgīti
  - vaitālīya
  - aupacchandasaka
