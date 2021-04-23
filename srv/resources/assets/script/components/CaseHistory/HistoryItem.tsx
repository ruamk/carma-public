import {h, FunctionalComponent, Fragment} from 'preact'
import {useRef} from 'preact/hooks'
import moment from 'moment'
import * as Type from './types'


type FC<T> = FunctionalComponent<T>


export const HistoryItem: FC<Type.HistoryItem>
  = ({timestamp, who, color, data}) =>
    <div className='well history-item' style={{backgroundColor: color}}>
      <div class='history-datetime'>{toTimestamp(timestamp)}</div>
      {who && <div class='history-who'>{who}</div>}
      <div class='history-body'>
        { data.type in components
          ? components[data.type](data)
          : (data.type || '--')
        }
      </div>
    </div>


const components: {[type: string]: any} = {
  action: ({actioncomment, actionresult, actiontype, servicelabel, tasks}: Type.Action) =>
    <Fragment>
      <NamedValue name='Действие' value={actiontype} icon='briefcase'/>
      <NamedValue name='Результат' value={actionresult}/>
      <NamedValue name='Услуга' value={servicelabel}/>
      {tasks &&
      <div>
        <b>Задачи:&nbsp;</b>
        {tasks.map(({isChecked, label}) =>
          <Fragment>
            <input type='checkbox' disabled={true} checked={isChecked}/>
            {label}
          </Fragment>
        )}
      </div>
      }
      <NamedValue name='Комментарий' value={actioncomment}/>
    </Fragment>,

  comment: ({commenttext}: Type.Comment) =>
    <NamedValue name='Комментарий' value={commenttext} icon='bullhorn'/>,

  call: ({calltype}: Type.Call) =>
    <NamedValue name='Звонок' value={calltype} icon='phone-alt'/>,

  customerFeedback: ({eventType, data}: Type.CustomerFeedback) => {
    if (eventType == 'FeedbackRequested') {
      return (
        <NamedIcon  name='Отправлена ссылка на опрос' icon='star'/>);
    }
    else if (eventType == 'FeedbackReceived') {
      const stars = n =>
        n && new Array(n).fill(0).map(_ =>
          <i class={'text-warning glyphicon glyphicon-star'}/>);
      return (
        <Fragment>
          <NamedIcon  name='Отзыв клиента' icon='star'/>
          <NamedValue name='Оценка оператору' value={stars(data.operValue)}/>
          <NamedValue name='Оценка механику' value={stars(data.techValue)}/>
          <NamedValue name='Комментарий' value={data.comment}/>
        </Fragment>);
    }
  },

  locationSharingRequest: () =>
    <NamedIcon
      name='Клиенту отправлено SMS с запросом местоположения'
      icon='map-marker'/>,

  locationSharingResponse: ({lat, lon}: Type.LocationSharingResponse) => {
    const lonlat = `${lon.toPrecision(7)},${lat.toPrecision(7)}`
    const mapUrl = `https://maps.yandex.ru/?z=18&l=map&pt=${lonlat}`
    return (
      <Fragment>
        <NamedIcon
          name='От клиента пришёл ответ с координатами:'
          icon='map-marker'/>&nbsp;
        <a href={mapUrl} target='_blank'>{lonlat}</a>
        <div style='float: right'>
          <CopyBtn value={mapUrl}>Скопировать в буфер обмена</CopyBtn>
        </div>
        <div style='clear:both'/>
      </Fragment>
    )
  },

  partnerCancel: ({refusalreason, refusalcomment, partnername}: Type.PartnerCancel) =>
    <Fragment>
      <NamedValue name='Отказ партнёра' value={partnername} icon='time'/>
      <NamedValue name='Причина отказа'
        value={`${refusalreason}\xa0${refusalcomment || ''}`}
      />
    </Fragment>,

  partnerDelay: ({delayconfirmed, delayminutes, partnername}: Type.PartnerDelay) =>
    <Fragment>
      <NamedValue name='Опоздание партнёра' value={partnername} icon='time'/>
      <NamedValue name='Время опоздания' value={delayminutes}/>
      <NamedValue name='Опоздание согласовано' value={delayconfirmed}/>
    </Fragment>,

  smsForPartner: ({deliverystatus, msgtext, phone, mtime}: Type.SmsForPartner) =>
    <Fragment>
      <NamedValue name='Партнёру отправлено SMS' value={msgtext} icon='envelope'/>
      <NamedValue name='Телефон получателя' value={phone}/>
      <NamedValue
        name='Статус отправки'
        value={`${deliverystatus} (обновлено: ${toTimestamp(mtime)})`}/>
    </Fragment>,

  avayaEvent: ({aetype, aeinterlocutors, aecall}: Type.AvayaEvent) =>
    <Fragment>
      <NamedValue name='Событие AVAYA' value={aetype} icon='earphone'/>
      <NamedValue name='Второй абонент' value={aeinterlocutors}/>
      <NamedValue name='Идентификатор звонка' value={aecall}/>
    </Fragment>,

  eraGlonassIncomingCallCard: ({requestId, requestBody: rq}: Type.EraGlonassIncomingCallCard) => {
    return (
      <div>
        <NamedIcon
          name='Поступление заявки на обслуживание от ЭРА-ГЛОНАСС.'
          icon='globe'/>
        <NamedValue name='Идентификатор заявки на обслуживание' value={requestId}/>
        <NamedValue name='Имя звонящего' value={rq?.fullName || '✗'}/>
        <NamedIcon name='Номера телефонов:' icon='earphone'/>
        <ul>
          <li>Терминал авто: {rq?.ivsPhoneNumber || '✗'}</li>
          <li>Звонящий: {rq?.phoneNumber || '✗'}</li>
        </ul>
        <div>
          <b>Транспорт:</b>
          <ul>
            <li>VIN: {rq?.vehicle?.vin || '✗'}</li>
            <li>Регистрационный номер: {rq?.vehicle?.plateNumber || '✗'}</li>
          </ul>
        </div>
        <NamedValue
          name='Описание местонахождения'
          value={rq?.location?.description || '✗'}/>
        <div>
          <NamedIcon name='Координаты:' icon='screenshot'/>
          <ul>
            <li><b>Широта</b>{toDegrees(rq?.location?.latitude)}</li>
            <li><b>Долгота</b>{toDegrees(rq?.location?.longitude)}</li>
          </ul>
        </div>
      </div>
    )
  }
}


// Helper functions and components

const toTimestamp = (date) =>
  moment.utc(date).local().format('DD.MM.YYYY HH:mm:ss')


const toDegrees = x => (x / (3600 * 1000)).toLocaleString('ru-RU', {
  minimumFractionDigits: 6,
  maximumFractionDigits: 6,
}) + '°'


type PropsItem = {
  name: string
  value?: any
  icon?: string
}


const NamedIcon: FC<PropsItem> = ({name, icon}) =>
  <div>
    <i class={`glyphicon glyphicon-${icon}`}/>&nbsp;
    <b>{name}</b>
  </div>


const NamedValue: FC<PropsItem> = ({name, value, icon}) =>
  value && (
    <div>
      {icon && <i class={`glyphicon glyphicon-${icon}`}/>}&nbsp;
      <b>{name}:</b>&nbsp;
      {value}
    </div>
  )


// see https://stackoverflow.com/questions/400212
const CopyBtn: FC<{value: string}> = ({value, children}) => {
  const textAreaRef = useRef(null)

  const copyToClipboard = e => {
    e.preventDefault()
    textAreaRef.current.select()
    document.execCommand('copy')
    e.target.focus()
  }

  return (
    <Fragment>
      <a href='#' onClick={copyToClipboard}>{children}</a>
      <textarea
        style='position: fixed; top: -999px; left: -999px'
        ref={textAreaRef}
        value={value}/>
    </Fragment>)
}
