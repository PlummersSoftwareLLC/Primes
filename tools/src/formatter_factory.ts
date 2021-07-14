import { IFormatter } from './formatter';
import { TableFormatter } from './formatters/table';
import { ChartFormatter } from './formatters/chart';
import { JSONFormatter } from './formatters/json';
import { CsvFormatter } from './formatters/text';

export default class FormatterFactory {
  static getFormatter(type: string): IFormatter {
    switch (type.toLocaleLowerCase()) {
      case 'table':
        return new TableFormatter();
      case 'chart':
        return new ChartFormatter();
      case 'json':
        return new JSONFormatter();
      case 'csv':
        return new CsvFormatter();
      default:
        return new TableFormatter();
    }
  }
}
