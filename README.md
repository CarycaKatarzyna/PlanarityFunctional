Projekt jest implementacją algorytmu Auslander-Parter-Goldstein, który służy do testowania planarności grafu.

Zaimplementowane rozwiązanie zakłada, że podany graf jest DWUSPÓJNY i NIESKIEROWANY. Jeśli użytkownik chce sprawdzić planarność grafu,
który nie jest dwuspójny, to powinien we własnym zakresie wygenerować podział na dwuspójne składowe i dla każdej z nich uruchomić program.
Jeżeli każda dwuspójna składowa jest planarna, to wejściowy graf też jest planarny.

Aby uruchomić program należy skompilować plik `Main.hs`. Po uruchomieniu programu należy wpisać ścieżkę do pliku z grafem (bez cudzysłowów).
Poprawny plik zawiera listę krawędzi grafu w postaci dwóch liczb całkowitych, a każda krawędź jest opisana w osobnej linii.
W folderze `examples` znajdują się przykłady poprawnie opisanych grafów.
Program wypisuje `True` jeśli graf jest planarny oraz `False` jeśli graf jest nieplanarny.
Program nie sprawdza poprawności ścieżki ani poprawności definicji grafu.
W szczególności podanie definicji grafu niedwuspójnego może zakończyć się błędem. 
