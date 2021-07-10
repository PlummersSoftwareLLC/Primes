import { Report, Result } from '../models';
import { IFormatter } from '../formatter';
import fs from 'fs';
import path from 'path';
import QuickChart from 'quickchart-js';

export class MarkdownFormatter implements IFormatter {
  render(report: Report): string {
    if (!fs.existsSync('../images')) {
      fs.mkdirSync('../images');
    }
    const markdownPath = '../RESULTS.md';
    const chartSingleThreadedLinearPath =
      '../images/chart_single_threaded_linear.png';
    const chartSingleThreadedLogarithmicPath =
      '../images/chart_single_threaded_logarithmic.png';
    const chartMultiThreadedLinearPath =
      '../images/chart_multi_threaded_linear.png';
    const chartMultiThreadedLogarithmicPath =
      '../images/chart_multi_threaded_logarithmic.png';

    const singleThreadedResults = report.results.filter(
      (result) => result.threads === 1
    );
    const multiThreadedResults = report.results.filter(
      (result) => result.threads > 1
    );

    const singleThreadedLinearChart = this.makeChart(
      singleThreadedResults,
      false
    );
    const singleThreadedLogarithmicChart = this.makeChart(
      singleThreadedResults,
      true
    );
    const multiThreadedLinearChart = this.makeChart(
      multiThreadedResults,
      false
    );
    const multiThreadedLogarithmicChart = this.makeChart(
      multiThreadedResults,
      true
    );

    singleThreadedLinearChart
      .toFile(chartSingleThreadedLinearPath)
      .catch((error) => console.error(error));
    singleThreadedLogarithmicChart
      .toFile(chartSingleThreadedLogarithmicPath)
      .catch((error) => console.error(error));
    multiThreadedLinearChart
      .toFile(chartMultiThreadedLinearPath)
      .catch((error) => console.error(error));
    multiThreadedLogarithmicChart
      .toFile(chartMultiThreadedLogarithmicPath)
      .catch((error) => console.error(error));

    let markdown = fs.readFileSync('template.md', 'utf-8');
    markdown = markdown.replace(
      '{chart_single_threaded_linear_path}',
      path.relative(path.dirname(markdownPath), chartSingleThreadedLinearPath)
    );
    markdown = markdown.replace(
      '{chart_single_threaded_logarithmic_path}',
      path.relative(
        path.dirname(markdownPath),
        chartSingleThreadedLogarithmicPath
      )
    );
    markdown = markdown.replace(
      '{chart_multi_threaded_linear_path}',
      path.relative(path.dirname(markdownPath), chartMultiThreadedLinearPath)
    );
    markdown = markdown.replace(
      '{chart_multi_threaded_logarithmic_path}',
      path.relative(
        path.dirname(markdownPath),
        chartMultiThreadedLogarithmicPath
      )
    );
    markdown = markdown.replace(
      '{systeminformation}',
      this.renderSysteminformation(report.machine)
    );
    fs.writeFileSync(markdownPath, markdown, 'utf-8');

    return `Markdown located at: ${markdownPath}`;
  }

  private renderSysteminformation(
    info: unknown,
    key?: string,
    space = 0
  ): string {
    const begin = key == undefined ? '' : `${Array(space).join('  ')}* ${key}:`;
    if (info === null || info === undefined) {
      return `${begin} unknown`;
    } else if (typeof info == 'object') {
      const obj = info as Record<string, unknown>;
      return `${begin}\n${Object.keys(obj)
        .map((key: string) =>
          this.renderSysteminformation(obj[key], key, space + 1)
        )
        .join('\n')}`;
    } else if (typeof info == 'boolean') {
      if (info) {
        return `${begin} True`;
      } else {
        return `${begin} False`;
      }
    }
    return `${begin} ${info}`;
  }

  private makeChart(results: Result[], logarithmic: boolean): QuickChart {
    results = results.sort(
      (a, b) =>
        b.passes / b.duration / b.threads - a.passes / a.duration / a.threads
    );
    const chart = new QuickChart();
    chart.setConfig({
      type: 'horizontalBar',
      options: {
        legend: {
          display: false
        },
        scales: {
          xAxes: [
            {
              type: logarithmic ? 'logarithmic' : 'linear',
              ticks: {
                beginAtZero: !logarithmic
              }
            }
          ]
        }
      },
      data: {
        labels: results.map(
          (result) =>
            `${result.label} (${result.implementation}, solution: ${result.solution})`
        ),
        datasets: [
          {
            data: results.map(
              (result) => result.passes / result.duration / result.threads
            )
          }
        ]
      }
    });
    chart.setWidth(838);
    chart.setHeight(results.length * 25 + 38);
    return chart;
  }
}
