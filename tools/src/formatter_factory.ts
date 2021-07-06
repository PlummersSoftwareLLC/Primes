import { IFormatter } from './formatter';
import { JSONFormatter } from './formatters/json';
import { TableFormatter } from './formatters/table';
import { CsvFormatter } from './formatters/text';

export default class FormatterFactory {
  static getFormatter(type: string): IFormatter {
    switch (type.toLocaleLowerCase()) {
      case 'json':
        return new JSONFormatter();
      case 'table':
        return new TableFormatter();
      case 'csv':
        return new CsvFormatter();
      default:
        return new TableFormatter();
    }
  }
}
