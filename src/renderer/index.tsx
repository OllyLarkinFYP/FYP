import React, { useState } from 'react';
import ReactDOM from 'react-dom';
import { hello } from './core/core.fs.js';

import './styles.scss';

const Entry = (props: { name: string }) => {
  const [count, incrementCount] = useState(0);

  return (
    <div className='hello'>
      <h1>{hello(props.name)}</h1>
      <h3>Counter: {count}</h3>
      <button
        onClick={() => incrementCount(count + 1)}
        className={count % 2 === 1 ? 'hello__btn--odd' : undefined}
      >
        Increment Counter
      </button>
    </div>
  );
};

// render `Entry` component
ReactDOM.render(<Entry name='React' />, document.getElementById('app'));
