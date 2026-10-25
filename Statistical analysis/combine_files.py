import csv
from collections import OrderedDict
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent.resolve()
DATA_DIR = SCRIPT_DIR.parent / 'Data'

INPUT_FILES = [
    DATA_DIR / 'bundle_auction_custom_export_2026-09-16.csv',
    DATA_DIR / 'bundle_auction_custom_export_2026-09-24.csv',
]

OUTPUT_FILE = DATA_DIR / 'combined_experiment_data.csv'

# Maintain insertion order of sections
sections = OrderedDict()
current_section_header = None

for file_path in INPUT_FILES:
    with open(file_path, mode='r', encoding='utf-8') as f:
        reader = csv.reader(f)
        for row in reader:
            if not row:
                continue
            
            # Identify section header rows (they start with 'record_type')
            if row[0] == 'record_type':
                current_section_header = tuple(row)
                if current_section_header not in sections:
                    sections[current_section_header] = []
            elif current_section_header:
                # Add data row to its respective section
                sections[current_section_header].append(row)

# Write the unified sections to the output file
with open(OUTPUT_FILE, mode='w', newline='', encoding='utf-8') as f:
    writer = csv.writer(f)
    for header, rows in sections.items():
        writer.writerow(header)
        writer.writerows(rows)

print(f"Successfully merged files into '{OUTPUT_FILE}'.")