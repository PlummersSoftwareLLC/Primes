import { Report } from './models';

export interface IFormatter {
  render(report: Report): string;
}
