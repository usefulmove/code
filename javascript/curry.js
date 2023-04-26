const curry = (fn) => {
  const curried = (...args) =>
    fn.length !== args.length ? curried.bind(null, ...args) : fn(...args);
  return curried;
};
