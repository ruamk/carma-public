
import moment from "moment"
import {h, render} from "preact"
import {useState} from "preact/hooks"
import cls from "classnames"
import {Spinner} from "carma/components/lib/Spinner"

import "./index.less"


const rootId = "quality-control-screen";
export const template = `<div id="${rootId}"/>`;

const toTimestamp = (date) =>
  moment.utc(date).local().format('DD.MM.YYYY HH:mm:ss')

const stars = n =>
  n && new Array(n).fill(0).map(_ =>
    <i class={'text-warning glyphicon glyphicon-star'}/>);


function ShortDetails({data}) {
  const operValue = data.find(e => !!e.operValue)?.operValue;
  const techValue = data.find(e => !!e.techValue)?.techValue;
  const comment = data.find(e => !!e.comment)?.comment;
  return (
    <table class="qualityControl-shortDetails">
      { operValue &&
        <tr>
          <th>Оператор:</th>
          <td>{stars(operValue)}</td>
        </tr>
      }
      { techValue &&
        <tr>
          <th>Техник:</th>
          <td>{stars(techValue)}</td>
        </tr>
      }
      { comment &&
        <tr>
          <td>{comment}</td>
        </tr>
      }
    </table>
 );
}

function FullDetails({data}) {
  const Event = ev =>
    <div className='well'>
      {toTimestamp(ev.ctime)} &nbsp; {ev.user.realName}
      { ev.operValue &&
        <div><b>Оператор:</b> &nbsp; {stars(ev.operValue)}</div>
      }
      { ev.techValue &&
        <div><b>Техник:</b> &nbsp; {stars(ev.techValue)}</div>
      }
      { ev.comment &&
        <div>{ev.comment}</div>
      }
    </div>;

  return (<div>{data.map(Event)}</div>);
}

function TableRow({data, active, onClick}) {
  return (
    <tr onClick={() => onClick(data)} class={cls({info: active})}>
      <th><a href={`#case/${data.caseId}`}>{data.caseId}</a></th>
      <td style={{textOverflow: 'ellipsis'}}>{data.program.label}</td>
      <td>{data.service ? data.service.type : '-'}</td>
      <td>
        <ShortDetails data={data.events} />
      </td>
    </tr>
  );
}

function Table({data, selected, onClick}) {
  return (
    <table class="table table-hover">
      <thead>
        <tr>
          <th>Кейс</th>
          <th>Программа</th>
          <th>Услуга</th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        {data.length > 0
          ? data.map(r =>
              <TableRow data={r} active={r == selected} onClick={onClick}/>
            )
          : <tr><td>Ничего нет</td></tr>
        }
      </tbody>
    </table>
  );
}



function Details({data}) {
  return (
    <form class="qualityControl-details">
      <div class="form-inline">
        <button type="button" class="btn btn-success">
          Проблема решена
        </button>
        <button type="button" class="btn btn-info">
          Оставить комментарий
        </button>
      </div>
      <div class="form-group">
        <textarea class="form-control" rows={3} placeholder="Комментарий">
        </textarea>
      </div>
      <FullDetails data={data.events} />
    </form>
  );
}


function Screen({data}) {
  const [currentRow, setCurrentRow] = useState(null);
  return (
    <div class="screen container-fluid row">
      <div class="col-md-8">
        <Table data={data} selected={currentRow} onClick={setCurrentRow}/>
      </div>
      <div class="col-md-4">
        {currentRow && <Details data={currentRow}/>}
      </div>
    </div>);
}


export async function constructor() {
  const root = document.getElementById(rootId);
  render(
    <h2><Spinner />&nbsp;Загрузка...</h2>,
    root);

  const r = await fetch("/customerFeedback?");

  if(!r.ok) {
    render(
      <h1>Ошибка при загрузке данных!</h1>,
      root);
  } else {
    const data = await r.json();
    render(<Screen data={data}/>, root);
  }
}


export function destructor() {
  render(null, document.getElementById(rootId));
}

// FIXME:
//   - SQL query filter by operValue & techValue
//   - add comment
//   - add status 'fixed'
