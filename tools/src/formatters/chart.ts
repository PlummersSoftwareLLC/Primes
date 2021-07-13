import { IFormatter } from '../formatter';
import { Report } from '../models';

import Chartscii, { ChartData } from 'chartscii';

export class ChartFormatter implements IFormatter {
  render(report: Report): string {
    const data: Array<ChartData> = report.results.map((result) => {
      return {
        label: `${result.implementation}-${result.solution} (${result.label})`,
        value: result.passes / result.duration / result.threads
      } as ChartData;
    });

    const chart = new Chartscii(data, {
      sort: true,
      naked: true
    });
    return chart.create();
  }
}
