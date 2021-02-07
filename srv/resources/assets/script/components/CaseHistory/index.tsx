import {h, FunctionalComponent} from 'preact'
import {useState} from 'preact/hooks'
import * as Type from './types'
import {HistoryItem} from './HistoryItem'
import {BtnGroup, BtnSpec} from './BtnGroup'

const filterSpec: BtnSpec[] = [
  { key: 'action',
    name: 'Показать действия',
    icon: 'briefcase' },
  { key: 'comment',
    name: 'Показать комментарии',
    icon: 'bullhorn' },
  { key: 'partnerCancel',
    name: 'Показать отказы партнёров',
    icon: 'remove-circle' },
  { key: 'partnerDelay',
    name: 'Показать опоздания партнёров',
    icon: 'time' },
  { key: 'call',
    name: 'Показать звонки',
    icon: 'phone-alt' },
  { key: 'smsForPartner',
    name: 'Показать SMS партнёрам',
    icon: 'envelope' },
  { key: 'eraGlonassIncomingCallCard',
    name: 'Показать поступления «Карточек Вызова» ЭРА-ГЛОНАСС',
    icon: 'globe' },
  { key: 'locationSharing',
    name: 'Показать запросы на определение координат',
    icon: 'map-marker' },
  { key: 'customerFeedback',
    name: 'Показать отзывы клиентов',
    icon: 'star' }
];


type Props = {
  items: () => Type.HistoryItem[]
}


export const CaseHistory: FunctionalComponent<Props> = ({items}) => {
  const [typeFilter, setTypeFilter] = useState<Set<string>>(
    // All type filters are active by default
    new Set(filterSpec.map(f => f.key))
  )
  const toggleFilter = (key: string) => {
    // FIXME: Good use case for immutable data structures like `immutable.js`.
    typeFilter.has(key) ? typeFilter.delete(key) : typeFilter.add(key)
    setTypeFilter(new Set(typeFilter))
  }

  const fullHistory = items()
  const filteredCaseHistory = fullHistory
    .filter(i =>
      (i.data.type === 'avayaEvent' && typeFilter.has('call'))
      || (i.data.type.startsWith('locationSharing') && typeFilter.has('locationSharing'))
      || typeFilter.has(i.data.type))

  return (
    <div id='case-history'>
      <h4 style='float: left'>История кейса</h4>
      <div style='float: right'>
        <BtnGroup
          buttons={filterSpec}
          activeButtons={typeFilter}
          onChange={toggleFilter}/>
      </div>
      <div style="clear:both"/>
      {filteredCaseHistory.map(HistoryItem)}
      <a class='more' href='#'>Ещё</a>
    </div>
  )
}
