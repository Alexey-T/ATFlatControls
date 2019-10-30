Sample code which creates control with two tabs:

    t:= TATTabs.Create(Self);
    t.Parent:= Self;
    t.Align:= alTop;                                   
    //optional event handlers
    t.OnTabClick:=     {$ifdef FPC}@{$endif} TabClick; 
    t.OnTabPlusClick:= {$ifdef FPC}@{$endif} TabPlusClick;
    t.OnTabClose:=     {$ifdef FPC}@{$endif} TabClose;
    //add tabs
    t.AddTab(-1, 'Tab one');
    t.AddTab(-1, 'Tab middle len', nil, false, clGreen);
