class ServerUtils {
  // false until the server answers (not logged as an error: it's expected while R starts)
  static async checkServerStatus(url, timeout = 3000) {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout);
    try {
      const res = await fetch(url, {
        method: "HEAD",
        signal: controller.signal,
      });
      return res.status === 200;
    } catch {
      return false;
    } finally {
      clearTimeout(timeoutId);
    }
  }

  static async waitFor(milliseconds) {
    return new Promise((resolve) => {
      setTimeout(resolve, milliseconds);
    });
  }

  static async waitForServerWithExponentialBackoff(
    url,
    maxAttempts = 10,
    initialDelay = 2000,
    maxDelay = 10000,
    backoffFactor = 1.5,
    statusCallback = null,
    shouldStop = null
  ) {
    let currentDelay = initialDelay;

    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      if (attempt > 1) {
        await this.waitFor(currentDelay);
      }
      // no point waiting for a server whose process has ended
      if (shouldStop && shouldStop()) {
        return false;
      }

      const isUp = await this.checkServerStatus(url);
      if (isUp) {
        const msg = `Server responded on attempt ${attempt} after ${currentDelay}ms delay`;
        console.log(msg);
        if (statusCallback) statusCallback(msg);
        return true;
      }

      currentDelay = Math.min(currentDelay * backoffFactor, maxDelay);

      const msg = `Server check attempt ${attempt}/${maxAttempts} failed. Next delay: ${currentDelay}ms`;
      console.log(msg);
      if (statusCallback) statusCallback(msg);
    }

    return false;
  }

  static async waitForServerWithJitter(
    url,
    maxAttempts = 10,
    baseDelay = 100,
    maxDelay = 3000
  ) {
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      if (attempt > 1) {
        const jitter = Math.random() * 0.3;
        const delay = Math.min(
          baseDelay * Math.pow(2, attempt - 2) * (1 + jitter),
          maxDelay
        );
        await this.waitFor(delay);
      }

      const isUp = await this.checkServerStatus(url);
      if (isUp) {
        return true;
      }
    }

    return false;
  }

  static async waitForServerSmart(
    url,
    strategy = "exponential",
    statusCallback = null,
    shouldStop = null
  ) {
    switch (strategy) {
      default:
      case "exponential":
        return await this.waitForServerWithExponentialBackoff(
          url,
          10,
          2000,
          10000,
          1.5,
          statusCallback,
          shouldStop
        );
      case "jitter":
        return await this.waitForServerWithJitter(url);
    }
  }
}

module.exports = ServerUtils;
