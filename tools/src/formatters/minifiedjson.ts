import { Report } from '../models';
import { IFormatter } from '../formatter';

export class MinifiedJSONFormatter implements IFormatter {
  render(report: Report): string {
    return JSON.stringify(report);
  }
}
