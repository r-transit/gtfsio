# https://stackoverflow.com/questions/48087762/markdown-table-to-data-frame-in-r
read_markdown_table = function(lines) {
	lines <- lines[!grepl('^[[:blank:]+-=:_|]*$', lines)]
	lines <- gsub('(^\\s*?\\|)|(\\|\\s*?$)', '', lines)
	readr::read_delim(paste(lines, collapse = '\n'), delim = '|',
					  trim_ws = TRUE, show_col_types = FALSE)
}

# parse all field definitions and return a list of tables
parse_fields = function(reference.md) {
	field_reference_list = list()

	ref_lines = readr::read_lines(reference.md)
	ref_lines[length(ref_lines)+1] <- "" # ensure empty last row

	table_index = stringr::str_starts(ref_lines, "\\| ") # lines with table markdown

	i = which(ref_lines == "## Field Definitions")
	while(i <= length(ref_lines)) {
		.line = ref_lines[i]
		if(stringr::str_starts(.line, "### ")) {
			.current_file <- stringr::str_replace_all(.line, "### ", "")
		}
		if(stringr::str_starts(.line, "File: ")) {
			.file_presence <- stringr::str_replace_all(.line, "File: ", "")
		}
		if(stringr::str_starts(.line, "Primary [kK]ey ")) {
			.primary_key <- stringr::str_replace_all(.line, "Primary [kK]ey \\(", "")
			.primary_key <- stringr::str_replace_all(.primary_key, "\\)", "")
		}

		# parse fields table
		if(stringr::str_starts(.line, "\\|[ ]+Field Name \\| Type")) {
			j = min(which(!table_index & seq_along(ref_lines) > i))-1
			ref_table = read_markdown_table(ref_lines[i:j])

			stopifnot(!is.null(.current_file), !is.null(.file_presence))
			if(is.null(.primary_key)) stopifnot(stringr::str_ends(.current_file, "geojson"))

			# print problems if available
			if(nrow(readr::problems(ref_table)) > 0) {
				cat(.current_file, "\n")
				print(readr::problems(ref_table)[,1:4])
			}

			# cleanup attributes
			attributes(ref_table)$presence <- .file_presence
			attributes(ref_table)$primary_key <- .primary_key
			attributes(ref_table)$spec <- NULL # remove col_type info
			attributes(ref_table)$problems <- NULL # remove col_type info

			# assign to return list
			field_reference_list[[.current_file]] <- ref_table

			# clear values
			.current_file <- .file_presence <- .primary_key <- NULL
		}
		i <- i+1
	}

	# Revision Date
	revision_date = gsub("**Revised ", "", ref_lines[3], fixed = T)
	revision_date <- readr::parse_date(strsplit(revision_date, "\\. See")[[1]][1], "%b %d, %Y")
	attributes(field_reference_list)$revision_date <- revision_date

	return(field_reference_list)
}

bind_fields_reference_list = function(field_reference_list) {
  field_reference_list |>
    bind_rows(.id = "File_Name") |>
    rename(Field_Name = `Field Name`) |>
    mutate(Field_Name = gsub("`", "", Field_Name)) |>
    mutate(Presence = gsub("**", "", Presence, fixed = TRUE))
}

parse_files = function(reference.md) {
  ref_lines = readr::read_lines(reference.md)

  i = which(ref_lines == "## Dataset Files")
  j = which(ref_lines == "## File Requirements")
  stopifnot(i < j)
  ref_lines <- ref_lines[i:j]

  index = stringr::str_starts(ref_lines, "\\| ")
  stopifnot(sum(diff(index)) == 0)

  files_table = read_markdown_table(ref_lines[index])

  files_table$`File Name` <- files_table$`File Name` |>
    strsplit(, split = "](", fixed = T) |>
    lapply(\(x) {
      gsub("[", "", x[1], fixed = T)
    }) |> unlist()

  return(files_table)
}

# cleanup
cleanup_files_reference = function(files_table) {
  files_table |>
    rename(File_Name = `File Name`) |>
    mutate(Presence = gsub("**", "", Presence, fixed = TRUE)) |>
    select(File_Name, File_Presence = Presence)
}
