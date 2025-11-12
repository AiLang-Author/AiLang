#!/bin/bash
# generate_web_forms.sh - Generate HTML forms from GUI definitions

DEFINITIONS_DIR="web_interface/definitions"
OUTPUT_DIR="web_interface/forms"

mkdir -p "$OUTPUT_DIR"

echo "=========================================="
echo "Web Form Generator"
echo "=========================================="
echo ""

for jsonfile in "$DEFINITIONS_DIR"/*.json; do
    if [ -f "$jsonfile" ]; then
        gui_name=$(basename "$jsonfile" _fields.json)
        
        echo "Generating form for: $gui_name"
        
        # Generate HTML form
        output_html="$OUTPUT_DIR/${gui_name}_form.html"
        
        cat > "$output_html" << 'HTMLHEAD'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FORM_TITLE</title>
    <style>
        body { font-family: Arial, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }
        h1 { color: #333; }
        .form-group { margin-bottom: 15px; }
        label { display: block; font-weight: bold; margin-bottom: 5px; }
        input[type="text"], input[type="number"] { width: 100%; padding: 8px; border: 1px solid #ddd; }
        input[type="checkbox"] { margin-right: 5px; }
        button { background: #007bff; color: white; padding: 10px 20px; border: none; cursor: pointer; }
        button:hover { background: #0056b3; }
        .field-hint { font-size: 0.9em; color: #666; }
    </style>
</head>
<body>
    <h1>FORM_TITLE</h1>
    <form id="gui-form" method="POST" action="/api/submit">
HTMLHEAD
        
        # Replace title
        sed -i "s/FORM_TITLE/Medicare ESRD - $gui_name/g" "$output_html"
        
        # Parse JSON and generate form fields
        python3 << PYPYTHON >> "$output_html"
import json
import sys

with open('$jsonfile', 'r') as f:
    data = json.load(f)

for field in data.get('fields', []):
    level = field.get('level')
    name = field.get('name')
    label = field.get('label', name)
    field_type = field.get('format', 'text')
    length = field.get('length', 0)
    default = field.get('default', '')
    
    # Skip high-level group items (01, 03)
    if level in [1, 3]:
        print(f'        <h3>{label}</h3>')
        continue
    
    # Skip 88-level conditionals for now
    if level == 88:
        continue
    
    print(f'        <div class="form-group">')
    print(f'            <label for="{name}">{label}</label>')
    
    if field_type == 'checkbox':
        checked = 'checked' if default else ''
        print(f'            <input type="checkbox" id="{name}" name="{name}" {checked}>')
    elif field_type == 'number' or field_type == 'decimal':
        print(f'            <input type="number" id="{name}" name="{name}" value="{default}">')
    else:
        maxlength = f'maxlength="{length}"' if length > 0 else ''
        print(f'            <input type="text" id="{name}" name="{name}" value="{default}" {maxlength}>')
    
    if length > 0:
        print(f'            <span class="field-hint">Max length: {length}</span>')
    
    print(f'        </div>')

PYPYTHON
        
        # Close HTML
        cat >> "$output_html" << 'HTMLFOOT'
        <button type="submit">Submit Claim</button>
    </form>
    
    <script>
        document.getElementById('gui-form').addEventListener('submit', async (e) => {
            e.preventDefault();
            const formData = new FormData(e.target);
            const data = Object.fromEntries(formData);
            
            const response = await fetch('/api/submit', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(data)
            });
            
            const result = await response.json();
            alert('Result: ' + JSON.stringify(result, null, 2));
        });
    </script>
</body>
</html>
HTMLFOOT
        
        echo "  ✓ $output_html"
    fi
done

echo ""
echo "Forms generated in: $OUTPUT_DIR"
echo ""
echo "To test: python3 -m http.server 8000"
echo "Then open: http://localhost:8000/web_interface/forms/"