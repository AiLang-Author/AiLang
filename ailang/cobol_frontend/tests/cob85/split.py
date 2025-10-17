import os

def split_cobol_file(file_path, output_dir):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line.startswith('*HEADER,COBOL,'):
            parts = line.split(',')
            if len(parts) >= 3:
                program_id = parts[2].strip()
            else:
                i += 1
                continue
            section_lines = [lines[i]]
            i += 1
            while i < len(lines):
                line = lines[i].strip()
                section_lines.append(lines[i])
                if line.startswith('*END-OF,') and len(line.split(',')) >= 2 and line.split(',')[1].strip() == program_id:
                    break
                i += 1
            output_file = os.path.join(output_dir, f"{program_id}.cbl")
            with open(output_file, 'w') as out:
                out.writelines(section_lines)
            print(f"Saved {output_file}")
        i += 1

# Example usage (replace with your paths)
split_cobol_file('newcob.cbl', 'split_cobol_tests')