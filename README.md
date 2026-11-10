# Website

Personal academic website for Mark G. Scott, built with [Quarto](https://quarto.org/) and hosted on GitHub Pages at <https://markgscott960.github.io/Website/>. Rendered output lives in `docs/`.

## Structure

- `_quarto.yml` - site configuration: navbar, footer, render list, `freeze: auto`.
- `theme.scss` - site theme layered on Bootswatch **Litera** (navy accent, Source Serif 4 headings, Inter body). To change the base theme, edit `format > html > theme` in `_quarto.yml`; do not set `theme:` in individual pages.
- `styles.css` - contact form and forecast page styles.
- `index.qmd` - About / home page (jolla template) with a featured-work listing.
- `projects.qmd` - **Research** listing page (card grid with category filters).
- `research/*.qmd` - one page per project. Front matter fields `description`, `image` and `categories` drive the cards. Status categories: `Complete`, `In progress`, `Planned`.
- `forecast.qmd`, `forecast-scorecard.qmd` - 2026 midterm forecasts and post-election scoring (`freeze: false`, so they always re-run).
- `DataVisualization.qmd` - visualization gallery (housing, health care, aging, presidential wealth).
- `methods.qmd` - coursework summaries (DACSS graduate courses, then an "Undergraduate Foundation" section for Montclair State / Penn State) and toolkit.
- `cv.qmd` - embedded CV (`files/resume.pdf`).
- `contactme.qmd`, `thank-you.qmd` - Formspree contact form and post-submission page.
- `VizCode/` - data and serialized model objects read by the pages (`Epstein/`, `Politics/`, `Census/`, `Forecast/`, `FlyReservoir/`).
- `img/thumbs/` - card thumbnails.

## Placeholders

Text still to be written is wrapped in `::: {.placeholder}` blocks (or `[text]{.ph}` inline); both render as yellow boxes. Find them with:

```
grep -rn "placeholder\|{.ph}" *.qmd research/*.qmd
```

## Rendering

```
quarto render
```

Output is written to `docs/`; commit `docs/` and `_freeze/`, then push to publish. With `freeze: auto`, pages only re-execute when their `.qmd` changes; render a single page (`quarto render research/epstein-network.qmd`) to force it.

The forecast scorecard reads `VizCode/Forecast/scores.rds`, written by `VizCode/Forecast/score_forecasts.R`. Re-run that script only when you want newer election results.

## Resources

- Bootswatch themes: <https://bootswatch.com/>
- Quarto website guide: <https://quarto.org/docs/websites/>
- Quarto listings: <https://quarto.org/docs/websites/website-listings.html>
