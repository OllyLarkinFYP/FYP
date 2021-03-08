import React, { useState } from 'react';
import ReactDOM from 'react-dom';

import './styles.scss';

const Entry = (props: { name: string }) => {
  const [count, incrementCount] = useState(0);

  return (
    <div className='hello'>
      <h1>Hello React!</h1>
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
