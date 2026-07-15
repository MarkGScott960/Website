# Website

Personal website for Mark G Scott, built with [Quarto](https://quarto.org/) and hosted on GitHub Pages (rendered output lives in `docs/`).

## Structure

- `_quarto.yml` — site configuration. Theme (`journal`) and `styles.css` are set here and apply site-wide; do not override `theme:` in individual pages.
- `index.qmd` — About Me / landing page
- `projects.qmd` — completed and planned research projects
- `DataVisualization.qmd` — data visualization portfolio (R / ggplot2 / plotly)
- `contactme.qmd` — Formspree-backed contact form
- `thank-you.qmd` — post-submission page for the contact form
- `styles.css` — site-wide custom CSS (contact form styling)
- `VizCode/` — data files used by DataVisualization.qmd
- `VizCodes.qmd` — scratch/draft code, excluded from rendering in `_quarto.yml`

## Rendering

```
quarto render
```

Output is written to `docs/`; commit and push to publish.

## Resources

- Bootswatch themes: <https://bootswatch.com/>
- Quarto website guide: <https://quarto.org/docs/websites/>
