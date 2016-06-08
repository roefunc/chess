jest.autoMockOff();

import TestUtils from 'react-addons-test-utils';

const ListData = require('../src/data').ListData;

describe('ListData', () => {
  it('has no lists when empty', () => {
    const list = new ListData();
    expect(list.getLists().length).toEqual(0);
  });
  it('returns a list that was added', () => {
    const list_data = new ListData();
    const list = list_data.addList("A List")
    expect(list_data.getLists().length).toEqual(1);
    expect(list.name).toEqual("A List");
  });
})
