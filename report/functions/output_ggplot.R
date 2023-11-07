# Show a ggplot2 chart in markdown output, save a ggplot2 chart
# and underlying data if needed

output_ggplot <-
  function(plot_object,
           plot_df,
           show_static,
           output_folder,
           save_csv,
           save_pdf,
           save_cairo = FALSE,
           pdf_width = 12,
           pdf_height = 8
           ) {
    # Get the name of the plot object
    plot_name <- deparse(substitute(plot_object))

    # Remove the "_plot" part of the name
    plot_name <- gsub("_plot", "", plot_name)

    if (show_static) {
      print(plot_object)
    }

    if (save_pdf) {
      ggsave(
        paste0(output_folder, '/', plot_name, '.pdf'),
        plot = plot_object,
        width = pdf_width,
        height = pdf_height
      )
    }

    if (save_cairo) {
      Cairo::CairoPDF(
        paste0(output_folder, '/', plot_name, '_cairo.pdf'),
        width = pdf_width,
        height = pdf_height
      )
      print(plot_object)
      dev.off()
    }

    if (save_csv) {
      plot_df |> readr::write_csv(paste0(output_folder, '/', plot_name, '.csv'), na = "")
    }

  }

