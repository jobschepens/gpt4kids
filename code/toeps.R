# --- CONFIGURATION ---
# PLEASE EDIT THESE TWO PATHS

# 1. SET THE PATH TO YOUR FOLDER CONTAINING PDFS
# Use forward slashes (/) or double backslashes (\\).
# pdf_folder_path <- "C:/Users/YourName/Documents/MyProject/Figures_PDF"
# pdf_folder_path <- "C:/Users/jobsc/Documents/GitHub/cdp-wp2/figures"
pdf_folder_path <- "C:/Users/jobsc/Documents/GitHub/gpt4kids/figures"

# 2. SET THE FULL PATH TO THE GHOSTSCRIPT EXECUTABLE YOU FOUND IN STEP 1
# This is the most important step! Make sure this path is correct.
ghostscript_exe_path <- "C:/Program Files/gs/gs10.05.1/bin/gswin64c.exe" 

# 3. SET THE NAME FOR THE SUBFOLDER WHERE EPS FILES WILL BE SAVED
eps_output_subfolder_name <- "eps_converted_ghostscript"

# --- SCRIPT ---

# Main conversion logic
convert_pdfs_to_eps_gs <- function(pdf_dir, gs_exe, eps_subdir_name) {
  # Check if Ghostscript executable exists at the provided path
  if (!file.exists(gs_exe)) {
    message("---------------------------------------------------------------------------------")
    message("ERROR: Ghostscript executable not found at the specified path:")
    message("  ", gs_exe)
    message("Please check the 'ghostscript_exe_path' variable in this script.")
    message("Follow Step 1 of the instructions to find the correct path.")
    message("---------------------------------------------------------------------------------")
    stop("Prerequisite 'gswin64c.exe' not found. Script aborted.")
  }
  
  # Normalize path and check if PDF directory exists
  pdf_dir <- normalizePath(pdf_dir, mustWork = FALSE)
  if (!dir.exists(pdf_dir)) {
    stop("The specified PDF folder does not exist: ", pdf_dir)
  }
  
  # Create output directory for EPS files
  eps_output_dir <- file.path(pdf_dir, eps_subdir_name)
  if (!dir.exists(eps_output_dir)) {
    dir.create(eps_output_dir, recursive = TRUE)
    message("Created output directory: ", eps_output_dir)
  } else {
    message("EPS files will be saved in existing directory: ", eps_output_dir)
  }
  
  # List all PDF files in the directory
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(pdf_files) == 0) {
    message("No PDF files found in: ", pdf_dir)
    return()
  }
  
  message("\nFound ", length(pdf_files), " PDF files to convert using Ghostscript.")
  success_count <- 0
  failure_count <- 0
  
  for (pdf_file_path in pdf_files) {
    pdf_filename <- basename(pdf_file_path)
    eps_filename <- sub("\\.pdf$", ".eps", pdf_filename, ignore.case = TRUE)
    eps_file_path <- file.path(eps_output_dir, eps_filename)
    
    message("\nConverting: ", pdf_filename, " -> ", eps_filename)
    
    # Arguments for Ghostscript to convert PDF to EPS
    # shQuote is used to handle spaces in file paths safely
    command_args <- c("-sDEVICE=eps2write",
                      "-dNOPAUSE",
                      "-dBATCH",
                      "-q", # Suppress startup messages
                      paste0("-sOutputFile=", shQuote(eps_file_path)),
                      shQuote(pdf_file_path))
    
    # Execute the command
    # status <- system2(command = shQuote(gs_exe), args = command_args, stdout = TRUE, stderr = TRUE)
    
    # For Windows, sometimes passing the command directly (if it has spaces)
    # without shQuote around the command itself is more reliable.
    # Arguments still need shQuote if they contain spaces (which pdf_file_path and eps_file_path might).
    status <- system2(command = gs_exe, args = command_args, stdout = TRUE, stderr = TRUE)
    
    
    # Check execution status
    if (is.null(attr(status, "status")) || attr(status, "status") == 0) {
      if (file.exists(eps_file_path) && file.info(eps_file_path)$size > 0) {
        message("  SUCCESS: Converted to ", eps_file_path)
        success_count <- success_count + 1
      } else {
        message("  WARNING: Command seemed to succeed but output file is missing or empty.")
        failure_count <- failure_count + 1
      }
    } else {
      message("  ERROR: Failed to convert ", pdf_filename)
      message("  Ghostscript exit status: ", attr(status, "status"))
      message("  Ghostscript output (stdout/stderr):\n", paste(status, collapse = "\n  "))
      failure_count <- failure_count + 1
    }
  }
  
  message("\n--- Conversion Summary ---")
  message("Successfully converted: ", success_count, " file(s).")
  message("Failed to convert: ", failure_count, " file(s).")
  message("EPS files are located in: ", normalizePath(eps_output_dir))
  message("--------------------------")
}

# --- RUN THE CONVERSION ---
# Make sure you have correctly set the two paths at the top of the script!
if (pdf_folder_path == "C:/Users/YourName/Documents/MyProject/Figures_PDF" || !dir.exists(pdf_folder_path)) {
  stop("Please set the 'pdf_folder_path' variable at the top of the script.")
} else {
  convert_pdfs_to_eps_gs(pdf_folder_path, ghostscript_exe_path, eps_output_subfolder_name)
}
