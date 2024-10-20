import child_process from 'child_process';

export default class DockerService {
  public buildImage(dirName: string, imageName: string): void {
    child_process.execSync(`docker build -t ${imageName} ${dirName}`, {
      stdio: 'pipe'
    });
  }

  public runContainer(imageName: string, duration: number, options: Array<string>): string {
    const output = child_process.execSync(`docker run --rm ${options.join(' ')} ${imageName}`, {
      stdio: 'pipe',
      timeout: duration ? duration * 60000 : undefined,
      killSignal: 'SIGKILL'
    });
    return output.toString('utf8');
  }
}
