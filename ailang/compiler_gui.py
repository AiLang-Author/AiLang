import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess
import os

class AILANGCompilerGUI:
    def __init__(self, root):
        # Fix for WSL2 X11 window disappearing issue
        # Force window to stay on top and handle focus
        self.root = root
        self.root.title("AILANG Compiler GUI")
        self.root.geometry("640x640")  # Minimum base size
        self.root.minsize(640, 640)    # Enforce minimum size
        self.root.attributes('-topmost', True)  # Keep window on top initially
        self.root.update()  # Ensure window is drawn
        self.root.attributes('-topmost', False)  # Release topmost after drawing
        self.root.protocol("WM_DELETE_WINDOW", self.on_closing)  # Handle window close

        self.debug_level = tk.StringVar(value="0")
        self.perf_enabled = tk.BooleanVar(value=False)
        self.perf_mode = tk.StringVar(value="")  # For --profile-cache, --profile-branch, --profile-all
        self.vm_mode = tk.StringVar(value="user")
        self.no_import_resolve = tk.BooleanVar(value=False)
        self.source_file = tk.StringVar(value="")
        self.output_file = tk.StringVar(value="")

        main_frame = tk.Frame(self.root, padx=15, pady=15)
        main_frame.pack(fill=tk.BOTH, expand=True)

        # Source file selection
        tk.Label(main_frame, text="Source File:").grid(row=0, column=0, sticky="w", pady=5)
        tk.Entry(main_frame, textvariable=self.source_file, width=50).grid(row=0, column=1, padx=5)
        tk.Button(main_frame, text="Browse", command=self.browse_file).grid(row=0, column=2, padx=5)

        # Output file
        tk.Label(main_frame, text="Output File:").grid(row=1, column=0, sticky="w", pady=5)
        tk.Entry(main_frame, textvariable=self.output_file, width=50).grid(row=1, column=1, padx=5)

        # Debug level
        tk.Label(main_frame, text="Debug Level:").grid(row=2, column=0, sticky="w", pady=5)
        debug_frame = tk.Frame(main_frame)
        debug_frame.grid(row=2, column=1, sticky="w")
        tk.Radiobutton(debug_frame, text="None", variable=self.debug_level, value="0").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(debug_frame, text="Level 1", variable=self.debug_level, value="1").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(debug_frame, text="Level 2", variable=self.debug_level, value="2").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(debug_frame, text="Level 3", variable=self.debug_level, value="3").pack(side=tk.LEFT, padx=2)

        # Performance profiling
        tk.Checkbutton(main_frame, text="Enable Performance Profiling", variable=self.perf_enabled).grid(row=3, column=0, columnspan=2, sticky="w", pady=5)
        perf_mode_frame = tk.Frame(main_frame)
        perf_mode_frame.grid(row=3, column=1, sticky="w", pady=5)
        tk.Radiobutton(perf_mode_frame, text="Default", variable=self.perf_mode, value="").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(perf_mode_frame, text="Cache", variable=self.perf_mode, value="cache").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(perf_mode_frame, text="Branch", variable=self.perf_mode, value="branch").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(perf_mode_frame, text="All", variable=self.perf_mode, value="all").pack(side=tk.LEFT, padx=2)

        # VM mode
        tk.Label(main_frame, text="VM Mode:").grid(row=4, column=0, sticky="w", pady=5)
        vm_frame = tk.Frame(main_frame)
        vm_frame.grid(row=4, column=1, sticky="w")
        tk.Radiobutton(vm_frame, text="User", variable=self.vm_mode, value="user").pack(side=tk.LEFT, padx=2)
        tk.Radiobutton(vm_frame, text="Kernel", variable=self.vm_mode, value="kernel").pack(side=tk.LEFT, padx=2)

        # No import resolve
        tk.Checkbutton(main_frame, text="Disable Import Resolve", variable=self.no_import_resolve).grid(row=5, column=0, columnspan=2, sticky="w", pady=5)

        # Compile button
        tk.Button(main_frame, text="Compile", command=self.compile, width=20).grid(row=6, column=0, columnspan=3, pady=15)

        # Quit button to close gracefully
        tk.Button(main_frame, text="Quit", command=self.root.quit, width=20).grid(row=7, column=0, columnspan=3, pady=5)

        # Output text area
        self.output_text = tk.Text(main_frame, height=20, width=70)
        self.output_text.grid(row=8, column=0, columnspan=3, pady=10)

    def on_closing(self):
        """Handle window close event to prevent accidental hiding."""
        if messagebox.askokcancel("Quit", "Do you want to quit?"):
            self.root.destroy()

    def browse_file(self):
        file_path = filedialog.askopenfilename(filetypes=[("AILANG and AIMacro files", "*.ailang;*.aimacro")])
        if file_path:
            self.source_file.set(file_path)
            output_name = file_path.replace('.ailang', '_exec').replace('.aimacro', '_exec')
            self.output_file.set(output_name)

    def compile(self):
        source_file = self.source_file.get()
        if not source_file or not os.path.exists(source_file):
            messagebox.showerror("Error", "Please select a valid source file.")
            return

        if source_file.endswith('.ailang'):
            compiler_script = "main.py"
        elif source_file.endswith('.aimacro'):
            compiler_script = "aimacro_frontend/integration.py"
        else:
            messagebox.showerror("Error", "Unsupported file type. Use .ailang or .aimacro.")
            return

        cmd = ["python3", compiler_script]
        debug_level = self.debug_level.get()
        if debug_level == "1":
            cmd.append("-D")
        elif debug_level == "2":
            cmd.append("-D2")
        elif debug_level == "3":
            cmd.append("-D3")

        if source_file.endswith('.ailang'):
            if self.perf_enabled.get():
                cmd.append("-P")
                perf_mode = self.perf_mode.get()
                if perf_mode == "cache":
                    cmd.append("-P:cache")
                elif perf_mode == "branch":
                    cmd.append("-P:branch")
                elif perf_mode == "all":
                    cmd.append("-P:all")
            cmd.extend(["--vm-mode", self.vm_mode.get()])
            if self.no_import_resolve.get():
                cmd.append("--no-import-resolve")
        elif source_file.endswith('.aimacro'):
            if int(debug_level) > 0:
                cmd.append("--debug")

        output_file = self.output_file.get()
        if output_file:
            cmd.extend(["-o", output_file])

        cmd.append(source_file)

        print(f"Executing: {' '.join(cmd)}")  # Debug output
        self.output_text.delete(1.0, tk.END)
        try:
            process = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=True
            )
            self.output_text.insert(tk.END, process.stdout)
            if process.stderr:
                self.output_text.insert(tk.END, "\nErrors:\n" + process.stderr)
            # Restore focus after compilation
            self.root.lift()
            self.root.focus_force()
        except subprocess.CalledProcessError as e:
            self.output_text.insert(tk.END, f"Compilation failed:\n{e.stderr}")
            self.root.lift()
            self.root.focus_force()
        except Exception as e:
            self.output_text.insert(tk.END, f"Unexpected error: {str(e)}")
            self.root.lift()
            self.root.focus_force()

if __name__ == "__main__":
    root = tk.Tk()
    app = AILANGCompilerGUI(root)
    root.mainloop()