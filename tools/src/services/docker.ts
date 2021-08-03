import child_process from 'child_process';

export default class DockerService {
  public buildImage(dirName: string, imageName: string): void {
    child_process.execSync(`docker build -t ${imageName} ${dirName}`, {
      stdio: 'pipe'
    });
  }

  public runContainer(imageName: string, unconfined: boolean): string {
    let options = '--rm';
    if (unconfined) {
      options += ' --security-opt seccomp=unconfined'
    }

    const output = child_process.execSync(`docker run ${options} ${imageName}`, {
      stdio: 'pipe'
    });
    return output.toString('utf8');
  }
}
