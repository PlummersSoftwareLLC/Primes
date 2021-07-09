import { Report } from '../models';
import { IFormatter } from '../formatter';

export class CsvFormatter implements IFormatter {
  render(report: Report): string {
    const results = report.results
      .map((value) => {
        return {
          ...value,
          passesPerSecond: value.passes / value.duration / value.threads
        };
      })
      .sort((a, b) => b.passesPerSecond - a.passesPerSecond);

    const csv = [
      [
        'Index',
        'Implementation',
        'Solution',
        'Label',
        'Passes',
        'Duration',
        'Threads',
        'Algorithm',
        'Faithful',
        'Bits',
        'PassesPerSecond'
      ],
      ...results.map((value, index) => [
        index + 1,
        value.implementation,
        value.solution,
        value.label,
        value.passes,
        value.duration.toFixed(5),
        value.threads,
        value.tags['algorithm'],
        value.tags['faithful'],
        value.tags['bits'],
        value.passesPerSecond.toFixed(5)
      ])
    ];

    return csv.map((value) => value.join(',')).join('\n');
  }
}
