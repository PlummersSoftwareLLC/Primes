import child_process from 'child_process';

export default class DockerService {
  public buildImage(dirName: string, imageName: string): void {
    child_process.execSync(`docker build -t ${imageName} ${dirName}`, {
      stdio: 'ignore'
    });
  }

  public runContainer(imageName: string): string {
    const output = child_process.execSync(`docker run --rm ${imageName}`, {});
    return output.toString('utf8');
  }
}
