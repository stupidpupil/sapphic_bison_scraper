get_sapphic_bison_test_kits <- function() {
	remDr <- get_selenium_session()

	remDr$navigate("https://sapphicbison.org/order")
	bttn <- remDr$findElement(using="xpath", "//button[text()=\"Order a Kit\"]")
	bttn$clickElement()

	kit_options <- remDr$findElements(using="xpath", "(//select)[1]/option")

	rets <- list()

	biomarkers <- readr::read_csv("data-raw/biomarkers.csv", col_types="cc")

	for (opt in kit_options) {
		ret <- list()
		opt_title <- opt$getElementText()
		opt$clickElement()
		opt_desc <- remDr$findElement(using="xpath", "//p[starts-with(., \"This kit\")]")$getElementText()


		regexp = "^(.+) Kit - £(\\d+(\\.\\d+)?)$"
		ret$name <- opt_title |> stringr::str_extract(regexp, group=1) |> jsonlite::unbox()
		ret$price_pence <- opt_title |> stringr::str_extract(regexp, group=2) |> as.double() * 100 |> as.integer() |> jsonlite::unbox()

		included_biomarkers <- biomarkers |>
			dplyr::filter(opt_desc |> stringr::str_to_lower() |> stringr::str_detect(biomarker_handle))

		ret$biomarkers <- included_biomarkers |> dplyr::pull("sctid")
		ret$sampling_procedure <- "fingerprick" |> jsonlite::unbox()
		ret$url <- "https://sapphicbison.org/order" |> jsonlite::unbox()

		rets <- append(rets, list(ret))
	}

	return(rets)
}