import csv
import re
# Définir les noms de fichiers
input_file = 'Jeux_Donnees/bodyfat.dat'
output_file = 'Jeux_Donnees/bodyfat.csv'

# Ouvrir le fichier source en lecture et le fichier de sortie en écriture
with open(input_file, 'r') as dat_file, open(output_file, 'w', newline='') as csv_file:
    csv_writer = csv.writer(csv_file, delimiter=',')
    
    for line in dat_file:
        # Utiliser une expression régulière pour diviser la ligne par des espaces multiples
        row = re.split(r'\s+', line.strip())
        # Écrire la ligne dans le fichier CSV avec des virgules comme séparateurs
        csv_writer.writerow(row)