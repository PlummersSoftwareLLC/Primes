import { Report } from '../models';
import { IFormatter } from '../formatter';

export class JSONFormatter implements IFormatter {
  render(report: Report): string {
    return JSON.stringify(report, undefined, 4);
  }
}
