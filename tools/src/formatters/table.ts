import { Table } from 'console-table-printer';
import { Report, Result } from '../models';
import { IFormatter } from '../formatter';

export class TableFormatter implements IFormatter {
  render(report: Report): string {
    const results = report.results;
    const output = new Array<string>();

    const singleThreadedResults = results.filter(
      (value) => value.threads === 1
    );
    if (singleThreadedResults.length > 0)
      output.push(this.printResults('Single-threaded', singleThreadedResults));

    const multiThreadedResults = results.filter((result) => result.threads > 1);
    if (multiThreadedResults.length > 0)
      output.push(this.printResults('Multi-threaded', multiThreadedResults));

    return output.join('\n');
  }

  private printResults(title: string, data: Result[]): string {
    const table = new Table({
      title,
      columns: [
        {
          name: 'index',
          title: 'Index',
          alignment: 'center',
          color: 'blue'
        },
        {
          name: 'implementation',
          title: 'Implementation',
          alignment: 'left'
        },
        { name: 'solution', title: 'Solution', alignment: 'left' },
        { name: 'label', title: 'Label', alignment: 'left' },
        { name: 'passes', title: 'Passes', alignment: 'center' },
        { name: 'duration', title: 'Duration', alignment: 'center' },
        { name: 'threads', title: 'Threads', alignment: 'center' },
        { name: 'algorithm', title: 'Algorithm', alignment: 'center' },
        { name: 'faithful', title: 'Faithful', alignment: 'center' },
        { name: 'bits', title: 'Bits', alignment: 'left' },
        {
          name: 'passesPerSecond',
          title: 'Passes/Second',
          alignment: 'center'
        }
      ]
    });

    table.addRows(
      data
        .map((value) => {
          return {
            ...value,
            passesPerSecond: value.passes / value.duration / value.threads
          };
        })
        .sort((a, b) => b.passesPerSecond - a.passesPerSecond)
        .map((value, index) => {
          return {
            index: index + 1,
            implementation: value.implementation,
            solution: value.solution,
            label: value.label,
            passes: value.passes,
            duration: value.duration.toFixed(5),
            threads: value.threads,
            algorithm: value.tags['algorithm'],
            faithful: value.tags['faithful'],
            bits: value.tags['bits'],
            passesPerSecond: value.passesPerSecond.toFixed(5)
          };
        })
    );

    return table.render();
  }
}
